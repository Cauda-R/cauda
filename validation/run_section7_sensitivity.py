"""Exploratory sensitivity analysis for Table 4 (Section 3.2 of the condensed paper).
Run from the repository root:  python validation/run_section7_sensitivity.py
Reproduces the primary Pearson results (first column) before the sensitivity variants.
Requires pandas, numpy, scipy.
"""
import pandas as pd, numpy as np
from scipy import stats
d=pd.read_csv('inst/extdata/opioid_real_data.csv')
pop={'AL':5024279,'AK':733391,'AZ':7151502,'AR':3011524,'CA':39538223,'CO':5773714,'CT':3605944,'DE':989948,'DC':689545,'FL':21538187,'GA':10711908,'HI':1455271,'ID':1839106,'IL':12812508,'IN':6785528,'IA':3190369,'KS':2937880,'KY':4505836,'LA':4657757,'ME':1362359,'MD':6177224,'MA':7029917,'MI':10077331,'MN':5706494,'MS':2961279,'MO':6154913,'MT':1084225,'NE':1961504,'NV':3104614,'NH':1377529,'NJ':9288994,'NM':2117522,'NY':20201249,'NC':10439388,'ND':779094,'OH':11799448,'OK':3959353,'OR':4237256,'PA':13002700,'RI':1097379,'SC':5118425,'SD':886667,'TN':6910840,'TX':29145505,'UT':3271616,'VT':643077,'VA':8631393,'WA':7705281,'WV':1793716,'WI':5893718,'WY':576851}
cols=['overdose_deaths_total','heroin_deaths','natural_semisynth_deaths','synthetic_opioid_deaths']
dd=d.dropna(subset=cols)   # R aggregate() formula default na.omit
agg=dd.groupby('state')[cols].mean()
pills=d.groupby('state')['arcos_total_pills_2006_2014'].mean()
agg['pills']=pills
agg=agg.loc[[s for s in agg.index if s in pop]]
agg['pop']=[pop[s] for s in agg.index]
print(len(agg), sorted(set(d.state)-set(agg.index)))
agg['heroin']=agg.heroin_deaths; agg['natsemi']=agg.natural_semisynth_deaths; agg['synth']=agg.synthetic_opioid_deaths
agg['heroin_r']=agg.heroin_deaths/agg['pop']*1e5; agg['natsemi_r']=agg.natural_semisynth_deaths/agg['pop']*1e5
agg['synth_r']=agg.synthetic_opioid_deaths/agg['pop']*1e5; agg['pills_r']=agg.pills/agg['pop']
region={}
NE='CT ME MA NH RI VT NJ NY PA'.split(); MW='IL IN MI OH WI IA KS MN MO NE ND SD'.split()
S='DE FL GA MD NC SC VA DC WV AL KY MS TN AR LA OK TX'.split(); W='AZ CO ID MT NV NM UT WY AK CA HI OR WA'.split()
for r,l in (('NE',NE),('MW',MW),('S',S),('W',W)):
    for s in l: region[s]=r
agg['region']=[region[s] for s in agg.index]
R=pd.get_dummies(agg.region,drop_first=True).astype(float).values
def resid(y,X):
    X=np.column_stack([np.ones(len(y))]+([X] if X is not None and X.size else []))
    b=np.linalg.lstsq(X,y,rcond=None)[0]; return y-X@b
def pcor(x,y,ctrl,df):
    # ctrl: array (n,k) or None; returns r, p (df = n-2-k)
    n=len(x); k=0 if ctrl is None else ctrl.shape[1]
    rx=resid(x,ctrl); ry=resid(y,ctrl); r=np.corrcoef(rx,ry)[0,1]
    dfree=n-2-k; t=r*np.sqrt(dfree/(1-r*r)); p=2*stats.t.sf(abs(t),dfree); return r,p
def tests(df,spec,method='pearson',region=False,log=False):
    sfx='_r' if spec=='rate' else ''
    h,nsm,sy,pl=(df['heroin'+sfx],df['natsemi'+sfx],df['synth'+sfx],df['pills_r' if spec=='rate' else 'pills'])
    arr=lambda s: np.log1p(s.values) if log else s.values.astype(float)
    h,nsm,sy,pl=map(arr,(h,nsm,sy,pl))
    if method=='spearman': h,nsm,sy,pl=[stats.rankdata(v) for v in (h,nsm,sy,pl)]
    Rg=None
    if region: Rg=pd.get_dummies(df.region,drop_first=True).astype(float).values
    def c(extra=None):
        if Rg is None and extra is None: return None
        parts=[v for v in (extra,Rg) if v is not None]
        return np.column_stack(parts)
    out={}
    out['Heroin_|_Nat/semi|Pills']=pcor(h,nsm,c(pl.reshape(-1,1)),0)
    out['Synth_|_Heroin']=pcor(sy,h,c(),0)
    out['Synth_|_Nat/semi']=pcor(sy,nsm,c(),0)
    out['Synth_|_Pills']=pcor(sy,pl,c(),0)
    return out
A=0.0125
rows=[]
for spec in ('raw','rate'):
    base=tests(agg,spec)
    print(spec,{k:(round(v[0],3),'%.2g'%v[1]) for k,v in base.items()})
    var={'Pearson':base,'Spearman':tests(agg,spec,'spearman'),'Log1p-Pearson':tests(agg,spec,log=True),'Region-adjusted':tests(agg,spec,region=True)}
    # leave-one-out
    loo={k:[] for k in base}
    for s in agg.index:
        sub=agg.drop(s); t=tests(sub,spec)
        for k,v in t.items(): loo[k].append(v)
    for k in base:
        row={'spec':spec,'test':k}
        for m,t in var.items(): row[m]=f"{t[k][0]:.2f} ({'V' if t[k][1]<A else 'n.s.'})"
        rs=[v[0] for v in loo[k]]; flips=sum((v[1]<A)!=(base[k][1]<A) for v in loo[k])
        row['LOO r range']=f"{min(rs):.2f} to {max(rs):.2f}"; row['LOO flips']=f"{flips}/48"
        rows.append(row)
res=pd.DataFrame(rows); pd.set_option('display.width',250); print(res.to_string())
res.to_csv('validation/output/section7_sensitivity.csv',index=False)
