import fs from 'node:fs/promises';
import path from 'node:path';
import {fileURLToPath,pathToFileURL} from 'node:url';
import {Presentation,PresentationFile} from '@oai/artifact-tool';
const B=path.dirname(fileURLToPath(import.meta.url)), W=path.dirname(B), A=path.resolve(W,'../analysis');
const SK='/Users/vivianpanghui/.codex/plugins/cache/openai-primary-runtime/presentations/26.1112.11217/skills/presentations';
const PY='/Users/vivianpanghui/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3';
const {finalizePresentation,applyPresentationChartFont}=await import(pathToFileURL(path.join(SK,'container_tools/artifact_tool_utils.mjs')));
const R=JSON.parse(await fs.readFile(path.join(B,'results.json'),'utf8'));
const M=JSON.parse(await fs.readFile(path.join(B,'main_slides.json'),'utf8'));
const lang=process.argv[2]||'en', zh=lang==='zh', font=zh?'Arial Unicode MS':'Arial';
const p=Presentation.create({slideSize:{width:1280,height:720}});
const C={bg:'#FAFAF7',ink:'#183547',muted:'#516774',accent:'#167D8D',orange:'#B87139',pale:'#EAF2F4',line:'#D6E0E4'};
const pick=(z,e)=>zh?z:e, num=(x,d=0)=>Number(x).toLocaleString('en-US',{minimumFractionDigits:d,maximumFractionDigits:d}), money=x=>'$'+num(x),pct=x=>num(Number(x)*100,1)+'%';
const tableSlides=[],chartSlides=[],manifest=[];
function txt(s,text,x,y,w,h,size=26,bold=false,color=C.ink,f=font){const o=s.shapes.add({geometry:'textbox',position:{left:x,top:y,width:w,height:h},fill:'none',line:{fill:'none',width:0}});o.text=text;o.text.style={fontFamily:f,fontSize:size,bold,color,insets:0,verticalAlignment:'top',autoFit:'none',wrap:'square'};if(size===12.5){o.text.style={wrap:'none',lineSpacing:1.0};}return o;}
function base(title,notes='',source='',main=false){const s=p.slides.add();s.background.fill=C.bg;const n=p.slides.items.length;txt(s,title,64,40,1152,100,title.length>64?38:42,true);txt(s,(main?String(n):pick('附录 ','Appendix ')+(n-10)),1108,674,106,24,14,false,C.muted);s.speakerNotes.text=notes+'\n\nSources: '+source;manifest.push({slide:n,title,main,source});return s;}
function foot(s,t){txt(s,t,64,636,1090,34,15,false,C.muted);}
function table(s,headers,rows,{x=64,y=164,w=1152,h=410,size=22,widths=null}={}){const v=[headers,...rows];const t=s.tables.add({rows:v.length,columns:headers.length,left:x,top:y,width:w,height:h,values:v,...(widths?{columnWidths:widths}:{})});t.cells.block({row:0,column:0,rowCount:v.length,columnCount:headers.length}).assign({textStyle:{fontFamily:font,fontSize:size,color:C.ink},margins:{left:12,right:12,top:8,bottom:8},anchor:'center'});t.borders.assign({fill:C.line,width:1,style:'solid'});for(let r=0;r<v.length;r++){for(let c=0;c<headers.length;c++){t.getCell(r,c).fill=r===0?C.pale:r%2===0?'#F1F5F6':'#FFFFFF';if(r===0)t.getCell(r,c).text.style={fontFamily:font,fontSize:size,bold:true,color:C.ink};}}tableSlides.push(p.slides.items.length);return t;}
function chart(s,type,categories,series,{x=64,y=180,w=760,h=390,title='',min=0,max=null,format='#,##0',legend=false}={}){const st={typeface:font,fontSize:19,fill:C.ink};const ch=s.charts.add(type,{position:{left:x,top:y,width:w,height:h},title,titleTextStyle:{...st,fontSize:23,bold:true},categories,series,hasLegend:legend,legend:{position:'bottom',textStyle:st},barOptions:{direction:'column',grouping:'clustered',gapWidth:75},chartFill:C.bg,plotAreaFill:C.bg,xAxis:{textStyle:st,majorGridlines:null,line:{fill:C.line,width:1}},yAxis:{min,...(max!==null?{max}:{}),numberFormatCode:format,textStyle:st,majorGridlines:{fill:C.line,width:1}},dataLabels:{showValue:true,position:'outEnd',numberFormatCode:format,textStyle:{...st,fontSize:19}}});applyPresentationChartFont(ch,{fontFamily:font});chartSlides.push(p.slides.items.length);return ch;}
function narrative(s,items,{x=64,y=170,w=1100,step=92,size=28}={}){items.forEach((t,i)=>txt(s,t,x,y+i*step,w,step-15,size,false));}
const cp=R['02_cluster_profiles'];const names=zh?cp.map(r=>r.name):['Younger, one product, low activity','Multiple products, moderate activity','One product, high activity','Multiple products, high activity','High recorded income and activity','Older, mainly one product'];
for(const m of M){
 const s=base(m.title[lang],pick('建议用时 ','Suggested time: ')+m.seconds+pick(' 秒\n',' seconds\n')+m.notes[lang],m.source,true);
 if(m.kind==='cover'){
  s.background.fill='#16384A';for(const sh of s.shapes.items){if(sh.text)sh.text.color='#FAFAF7';}
  // Replace the usual header geometry with a generous cover title.
  s.shapes.items[0].position={left:64,top:140,width:1135,height:220};s.shapes.items[0].text.fontSize=58;
  txt(s,'BUSINFO 700   Assignment 3',64,68,1100,48,24,false,'#D6E5EB');
  txt(s,m.body[lang][0],64,410,1100,58,30,false,'#FFFFFF');txt(s,m.body[lang][1],64,490,1100,100,30,false,'#D6E5EB');
 }else if(m.kind==='audit'){
  txt(s,'1,689',64,164,480,95,76,true,C.accent);txt(s,m.body[lang][0],64,270,490,65,26);
  txt(s,'4,368',688,164,490,95,76,true,C.orange);txt(s,m.body[lang][1],688,270,490,65,26);
  narrative(s,m.body[lang].slice(2),{y:402,step:85,size:28});foot(s,pick('10,000 条记录，7 个变量。Income 和 No_Trans 的记录期间未注明。','10,000 records and seven variables. Income and transaction reporting periods are unspecified.'));
 }else if(m.kind==='agechart'){
  const age=R['01_age_band_profile'];chart(s,'bar',age.map(r=>r.group),[{name:pick('平均记录 CLV','Mean recorded CLV'),values:age.map(r=>+r.CLV_mean),fill:C.accent}],{w:740,title:pick('各年龄组平均记录 CLV（$）','Mean recorded CLV by age band ($)'),y:165,h:395,max:2700});
  narrative(s,m.body[lang],{x:850,y:190,w:356,step:115,size:26});foot(s,pick('年轻组未调整均值差约 −$833。差异反映旧估计标签。','Unadjusted under-30 mean gap is approximately −$833. These are differences in old estimates.'));
 }else if(m.kind==='selection'){
  narrative(s,m.body[lang].slice(0,2),{y:152,step:65,size:26});
  table(s,[pick('候选方案','Candidate'),pick('精确 silhouette','Exact silhouette'),pick('最小群人数','Smallest group')],[[pick('两群备选','Two-group alternative'),'0.3205','3,349'],[pick('六群主方案','Six-group primary'),'0.3163','470']],{y:312,h:170,size:25,widths:[520,320,312]});
  txt(s,m.body[lang][2],64,517,1140,75,27);foot(s,m.body[lang][3]);
 }else if(m.kind==='profiles'){
  table(s,[pick('群体','ID'),pick('描述','Profile'),pick('人数','n'),pick('年龄均值','Mean age'),pick('交易均值','Mean trans.')],cp.map((r,i)=>['C'+(i+1),names[i],num(r.n),num(r.Age_mean),num(r.No_Trans_mean)]),{y:155,h:440,size:23,widths:[80,532,160,180,200]});foot(s,m.body[lang][0]);
 }else if(m.kind==='model'){
  txt(s,'0.528',64,160,440,94,76,true,C.accent);txt(s,pick('测试 R²','Test R²'),64,270,440,48,28);
  txt(s,'$486',690,160,450,94,76,true,C.accent);txt(s,pick('平均绝对误差','Mean absolute error'),690,270,470,48,28);
  narrative(s,m.body[lang].slice(2),{y:380,step:95,size:28});foot(s,pick('训练 8,003 条，测试 1,997 条。R² 不是“预测准确率”。','8,003 training records and 1,997 test records. R² is not prediction accuracy.'));
 }else if(m.kind==='limitations'){
  narrative(s,m.body[lang],{y:158,step:106,size:29});foot(s,pick('HC3 调整系数不确定性，不修复尾部预测，也不建立因果关系。','HC3 adjusts coefficient uncertainty. Tail prediction and causality remain separate issues.'));
 }else if(m.kind==='profit'){
  chart(s,'bar',cp.map((_,i)=>'C'+(i+1)),[{name:'Yield',values:cp.map(r=>+r.Yield_mean),fill:C.orange}],{x:64,y:165,w:550,h:340,title:pick('当前平均 Yield（$）','Mean current Yield ($)'),max:350});
  chart(s,'bar',cp.map((_,i)=>'C'+(i+1)),[{name:'CLV',values:cp.map(r=>+r.CLV_mean),fill:C.accent}],{x:664,y:165,w:550,h:340,title:pick('平均记录 CLV（$）','Mean recorded CLV ($)'),max:5500});
  txt(s,m.body[lang][0]+'\n'+m.body[lang][1],64,535,1120,75,27);foot(s,m.body[lang][2]);
 }else if(m.kind==='actions'){
  const heads=zh?['行动与群体','画像与模型证据','评价方式']:['Action and segment','Profile and model evidence','Evaluation'];
  const rows=zh?[
   ['C5 关系维护','CLV $4,741；产品与交易系数为正','留存、净 Yield、投诉'],
   ['C3 产品试点','1 个产品，372 次交易；产品关联 +$652','适配采纳、净 Yield'],
   ['C3 成本调查','56.2% 亏损；Yield 交易系数为负，模型弱','交易成本、服务可达性']]:[
   ['C5 relationship support','CLV $4,741. Positive product and activity coefficients.','Retention, net Yield and complaints'],
   ['C3 product pilot','One product, 372 transactions. Product association +$652.','Suitable adoption and net Yield'],
   ['C3 cost investigation','56.2% losses. Negative transaction coefficient in a weak Yield model.','Transaction cost and service access']];
  table(s,heads,rows,{y:170,h:390,size:24,widths:[290,540,322]});foot(s,pick('使用试点与对照比较。系数不能当作增量收益承诺。','Use controlled pilots. Coefficients are not promises of incremental returns.'));
 }else if(m.kind==='conclusion'){
  narrative(s,m.body[lang],{y:158,step:108,size:29});foot(s,pick('下一步：核实记录，执行小试点，再用真实后续收益评价。','Next: verify records, run controlled pilots, then evaluate realised outcomes.'));
 }
}
function appendix(titleZ,titleE,source){return base(pick(titleZ,titleE),pick('技术备查，不计入十分钟主讲。','Technical reference, outside the ten-minute talk.'),source);}
function resultTable(tz,te,key,headers,cols,formats={},opts={}){const s=appendix(tz,te,'analysis/tables/'+key+'.csv');const rows=R[key].map(r=>cols.map(c=>formats[c]?formats[c](r[c]):r[c]));table(s,headers,rows,{size:20,h:410,...opts});foot(s,pick('来源：','Source: ')+key+'.csv');return s;}
const ds=appendix('变量字典与分析角色','Variable dictionary and analytical roles','01_data_dictionary.csv; assignment requirement.docx');
table(ds,pick(['变量','类型与含义','用途'],['Variable','Type and meaning','Use']),[
 ['Tot_Prod',pick('产品计数，实际范围 1–5','Product count, observed range 1–5'),pick('聚类与回归','Clustering and regression')],
 ['Income',pick('记录收入，零值含义不明','Recorded income, ambiguous zeros'),pick('聚类与公式敏感性','Clustering and provenance checks')],
 ['No_Trans',pick('交易次数','Transaction count'),pick('聚类与回归','Clustering and regression')],
 ['Age',pick('年龄，18–99 岁','Age, 18–99 years'),pick('聚类与回归','Clustering and regression')],
 ['Cat_Age',pick('有序年龄类别','Ordered age category'),pick('画像，不重复作为输入','Profiling, excluded as redundant input')],
 ['Yield',pick('当前利润，可为负','Current profit, can be negative'),pick('被动画像与补充回归','Passive profile and secondary regression')],
 ['CLV',pick('旧公式生成的预期价值','Old formula-based expected value'),pick('被动画像与主回归结果','Passive profile and primary outcome')]
],{size:20,y:160,h:435,widths:[160,470,522]});
resultTable('原始数据的描述统计','Descriptive statistics of original data','01_univariate_summary',pick(['变量','均值','中位数','标准差','最小','最大'],['Variable','Mean','Median','SD','Min','Max']),['variable','mean','median','sd','min','max'],Object.fromEntries(['mean','median','sd','min','max'].map(k=>[k,x=>num(x,2)])),{size:21,h:385});
{
 const s=appendix('数据处理与零收入检查','Data treatment and zero-income checks','01_data_audit.csv; 01_zero_income_profile.csv; 02_cluster_input_caps.csv');
 table(s,pick(['项目','结果与处理'],['Issue','Finding and treatment']),[
 [pick('零收入','Zero income'),pick('1,689 条；其中 1,576 条有交易。保留并比较替代处理。','1,689 records, including 1,576 with transactions. Retain and test alternatives.')],
 [pick('负利润','Negative Yield'),pick('4,368 条。题目允许，不删除。','4,368 records. Valid under the brief, retained.')],
 [pick('重复数值行','Repeated value rows'),pick('2 条。无客户 ID，不能确认重复客户，保留。','Two repeated value rows. No IDs to establish duplicate people, retained.')],
 [pick('聚类上端阈值','Clustering upper caps'),'Income 5,375.11; No_Trans 702.04'],
 [pick('Age 与 Cat_Age','Age and Cat_Age'),pick('类别 2:18–35，3:36–50，4:51–64，5:65–79，6:80–99','Categories 2:18–35, 3:36–50, 4:51–64, 5:65–79, 6:80–99')]
 ],{h:420,size:22,widths:[270,882]});foot(s,pick('主数据和盈利结果保持原样。阈值只作用于聚类工作副本。','Original records and outcomes remain unchanged. Caps apply only to clustering inputs.'));
}
{
 const s=appendix('双变量结果与检验','Bivariate results and tests','01_correlations_tests.csv; 01_zero_income_chisquare.csv; 01_welch_two_group_tests.csv; 01_age_band_anova.csv');
 const rs=R['01_correlations_tests'].filter(r=>r.y==='CLV'&&r.method==='pearson');table(s,pick(['变量与 CLV','Pearson r','Spearman r'],['Variable vs CLV','Pearson r','Spearman r']),rs.map(r=>[r.x,num(r.r,3),num(R['01_correlations_tests'].find(z=>z.x===r.x&&z.y==='CLV'&&z.method==='spearman').r,3)]),{y:155,h:260,size:22});
 txt(s,pick('年龄带 × 零收入：χ²(5)=294.70，p<0.001，Cramér’s V=0.172\n年轻组 CLV 均值差 −$832.77，95% CI [−$879.11, −$786.43]\n年龄带 CLV：Welch F=269.15，p<0.001，描述性 η²=0.075','Age band × zero income: χ²(5)=294.70, p<0.001, Cramér’s V=0.172\nUnder-30 CLV mean gap −$832.77, 95% CI [−$879.11, −$786.43]\nAge-band CLV: Welch F=269.15, p<0.001, descriptive η²=0.075'),64,450,1148,155,23);
}
resultTable('候选群数比较','Candidate cluster comparison','02_k_comparison',pick(['k','抽样 silhouette','全体 silhouette','最小群','组内平方和'],['k','Sample silhouette','Full silhouette','Smallest n','Within SS']),['k','sampled_silhouette','full_silhouette','smallest_n','within_ss'],{sampled_silhouette:x=>num(x,4),full_silhouette:x=>x==null||x===''||x==='NA'?'—':num(x,4),smallest_n:x=>num(x),within_ss:x=>num(x,1)},{size:21,h:420});
{
 const s=appendix('六群完整均值画像','All original numeric means by cluster','02_cluster_profiles.csv');table(s,pick(['群','人数','产品','收入','交易','年龄','Yield','CLV'],['ID','n','Products','Income','Trans.','Age','Yield','CLV']),cp.map((r,i)=>['C'+(i+1),num(r.n),num(r.Tot_Prod_mean,2),num(r.Income_mean),num(r.No_Trans_mean),num(r.Age_mean,1),num(r.Yield_mean,1),num(r.CLV_mean)]),{size:22,y:170,h:395,widths:[75,125,150,160,150,140,150,202]});foot(s,pick('全部均值来自原始未截尾数据。金额按原始记录单位。','All means use original uncapped data and recorded monetary units.'));
}
{
 const s=appendix('年龄类别的群内比例','Age-category proportions within clusters','02_cluster_age_category_proportions.csv');const rows=R['02_cluster_age_category_proportions'].map(r=>['C'+r[''],...['1','2','3','4','5','6'].map(k=>pct(r[k]))]);table(s,['ID','Cat 1','Cat 2','Cat 3','Cat 4','Cat 5','Cat 6'],rows,{size:22,h:395});foot(s,pick('各行合计约 100%（四舍五入）；Cat 1 在样本中不存在。','Rows total approximately 100% after rounding. Category 1 is absent.'));
}
{
 const s=appendix('盈利中位数与零收入比例','Profit medians and zero-income rates','02_cluster_profiles.csv');table(s,pick(['群','占比','Yield 中位','CLV 中位','亏损率','零收入率'],['ID','Share','Median Yield','Median CLV','Loss rate','Zero-income rate']),cp.map((r,i)=>['C'+(i+1),pct(r.share),num(r.Yield_median),num(r.CLV_median),pct(r.negative_yield_rate),pct(r.zero_income_rate)]),{size:22,h:395});
}
{
 const s=appendix('分群稳定性与处理敏感性','Cluster stability and preprocessing sensitivity','02_seed_stability.csv; 02_preprocessing_sensitivity.csv');const sr=R['02_preprocessing_sensitivity'];const map=zh?['不截尾','截尾并对收入取对数','零值暂作缺失并填补','去除 Income']:['No caps','Capped and log income','Zero treated as missing and imputed','Income excluded'];table(s,pick(['方案','相对主方案 ARI','最小群人数'],['Alternative','ARI vs primary','Smallest group']),sr.map((r,i)=>[map[i],num(r.adjusted_rand_vs_primary,3),num(r.smallest_n)]),{size:23,h:305,widths:[650,260,242]});txt(s,pick('换种子 ARI 约 0.995–1.000。\n处理方式改变会影响客户归属，特别是收入的处理。','Seed-change ARIs are approximately 0.995–1.000.\nPreprocessing choices, especially income treatment, change membership.'),64,505,1130,100,26);foot(s,pick('不同处理空间内的 silhouette 不宜直接作优劣比较。','Silhouettes from different preprocessing spaces are not directly comparable.'));
}
resultTable('训练集内部模型选择','Training-only model selection','03_CV_model_comparison',pick(['方案','CV RMSE','CV MAE','参数数'],['Candidate','CV RMSE','CV MAE','Parameters']),['model','pooled_RMSE','weighted_MAE','parameters'],{pooled_RMSE:x=>num(x,2),weighted_MAE:x=>num(x,2)},{size:24,h:250});
{
 const s=appendix('主 CLV 回归输出','Primary CLV regression output','03_primary_training_fit.csv; 03_primary_coefficients_HC3.csv; models/primary_summary.txt');const cf=R['03_primary_coefficients_HC3'];table(s,pick(['项','系数','HC3 标准误','95% HC3 下限','95% HC3 上限'],['Term','Estimate','HC3 SE','95% HC3 lower','95% HC3 upper']),cf.map(r=>[r.term,num(r.estimate,4),num(r.HC3_se,4),num(r.HC3_low95,4),num(r.HC3_high95,4)]),{size:21,h:290});txt(s,'CLV = −272.7273 + 652.2724 Tot_Prod + 4.9733 No_Trans + 8.9427 (Age−45)\n'+pick('训练 R²=0.5067，调整 R²=0.5065，F(3,7999)=2738.77，p<0.001\n残差标准误 $982.60；全部斜率 HC3 p<0.001','Training R²=0.5067, adjusted R²=0.5065, F(3,7999)=2738.77, p<0.001\nResidual SE $982.60. All slope HC3 p-values <0.001'),64,493,1140,120,22);
}
{
 const s=appendix('测试集表现与模型敏感性','Held-out performance and sensitivity','03_primary_test_metrics.csv; 03_Yield_test_metrics.csv; 03_CLV_target_provenance_sensitivity.csv');const cl=R['03_primary_test_metrics'][1],yl=R['03_Yield_test_metrics'][1];table(s,pick(['预测对象','MAE $','RMSE $','测试 R²'],['Outcome','MAE $','RMSE $','Test R²']),[['CLV',num(cl.MAE,2),num(cl.RMSE,2),num(cl.R2,3)],['Yield',num(yl.MAE,2),num(yl.RMSE,2),num(yl.R2,3)]],{h:190,size:26});txt(s,pick('CLV 训练均值基准 RMSE=$1,332.33\n加入 Income：测试 R²≈0.708\n更广泛旧公式诊断：测试 R²≈0.960\n这些改进衡量旧标签拟合，不验证真实未来价值。','CLV training-mean baseline RMSE=$1,332.33\nAdd Income: test R²≈0.708\nBroader formula diagnostic: test R²≈0.960\nThese improvements measure old-label fit, not genuine future value.'),64,418,1130,185,27);
}
{
 const s=appendix('年轻客户专题与补充 Yield 模型','Under-30 diagnostic and secondary Yield model','03_under30_adjusted_association.csv; 03_Yield_coefficients_HC3.csv; models/yield_summary.txt');table(s,pick(['年轻组专题模型','Under30 系数','95% HC3 区间'],['Under-30 diagnostic','Under30 coefficient','95% HC3 interval']),[
 [pick('产品、交易、年龄二次项','Products, transactions and quadratic age'),'−$197.09','[−$272.58, −$121.60]'],
 [pick('再加入 Income 与 Yield','Also add Income and Yield'),'−$29.12','[−$54.68, −$3.56]']
 ],{size:22,h:190,widths:[650,240,262]});txt(s,pick('补充 Yield 模型使用产品数、交易、年龄、收入与零收入指示。\n交易系数约 −0.56；测试 R²=0.060。\n完整输出见随后代码和保存的 yield_summary.txt。\n年轻组差异不是与真实未来价值比较的低估量。','The secondary Yield model uses products, transactions, age, income and a zero-income indicator.\nTransaction coefficient ≈−0.56. Test R²=0.060.\nFull output is retained in yield_summary.txt.\nThe age gap does not measure underestimation against true future value.'),64,414,1140,185,24);
}
// Full secondary coefficients as relevant model output.
resultTable('补充 Yield 模型的系数','Secondary Yield model coefficients','03_Yield_coefficients_HC3',pick(['项','系数','HC3 标准误','95% 下限','95% 上限'],['Term','Estimate','HC3 SE','95% lower','95% upper']),['term','estimate','HC3_se','HC3_low95','HC3_high95'],Object.fromEntries(['estimate','HC3_se','HC3_low95','HC3_high95'].map(k=>[k,x=>num(x,4)])),{size:20,h:395});
for(const [file,tz,te,caption] of [
 ['09_regression_diagnostics.png','CLV 模型诊断','CLV model diagnostics',pick('残差长尾、异方差；最大 VIF≈1.12。HC3 调整推断，不能修复个体预测。','Long-tailed and unequal errors. Maximum VIF≈1.12. HC3 adjusts inference, not predictions.')],
 ['10_test_predictions.png','CLV 测试预测','CLV held-out predictions',pick('左图全范围，右图放大至第 99 百分位；全部测试记录进入评价指标。','Full range at left, 99th-percentile zoom at right. Metrics retain all test records.')]
]){const s=appendix(tz,te,'analysis/plots/'+file+'; 03_primary_diagnostics.csv');const b=await fs.readFile(path.join(A,'plots',file));s.images.add({blob:b.buffer.slice(b.byteOffset,b.byteOffset+b.byteLength),contentType:'image/png',alt:te,fit:'contain',position:{left:64,top:150,width:1152,height:465}});foot(s,caption);}
{
 const s=appendix('课程对应与复现说明','Course references and reproducibility','W3–W8 lecture decks; analysis/START_HERE.txt');narrative(s,pick([
 'W3 第 7–11、20–35 页：变量类型、描述统计与 z-score',
 'W4 第 11–27 页，W6 第 9–24 页：检验、关联与组间比较',
 'W7 第 15–21、24–30 页：标准化、群数选择与画像',
 'W8 第 20–30 页：回归解释、拟合与诊断',
 '以下附录保留全部 R 源码。独立运行文件位于 group/analysis。'],[
 'W3 slides 7–11 and 20–35: types, summaries and z-scores',
 'W4 slides 11–27 and W6 slides 9–24: tests and associations',
 'W7 slides 15–21 and 24–30: scaling, k selection and profiles',
 'W8 slides 20–30: regression interpretation, fit and diagnostics',
 'The following appendix contains all R source. Runnable files remain in group/analysis.'
 ]),{y:160,step:88,size:25});}
// Include every R line, preserving filenames and line numbers. Wrapping is display only.
const sources=['run_analysis.R','R/00_helpers.R','R/01_data_eda.R','R/02_clusters.R','R/03_models.R','R/04_report.R','verify_results.R'];
function wrapUnits(text,max=75){const out=[];let cur='',n=0;for(const c of text){const w=c.codePointAt(0)>255?2:1;if(n+w>max){out.push(cur);cur='     ';n=5;}cur+=c;n+=w;}out.push(cur);return out;}
const codeManifest=[];
for(const file of sources){const raw=await fs.readFile(path.join(A,file),'utf8');const lines=raw.replace(/\n$/,'').split('\n');const wrapped=[];lines.forEach((line,i)=>wrapped.push(...wrapUnits(String(i+1).padStart(3)+' '+line)));
 for(let start=0;start<wrapped.length;start+=60){const end=Math.min(start+60,wrapped.length);const s=appendix(pick('R 代码 ','R code ')+file+`  ${Math.floor(start/60)+1}`,pick('R code ','R code ')+file+`  ${Math.floor(start/60)+1}`,'analysis/'+file);const page=wrapped.slice(start,end);txt(s,page.slice(0,30).join('\n'),48,152,580,493,12.5,false,'#142F40','Arial Unicode MS');txt(s,page.slice(30).join('\n'),656,152,580,493,12.5,false,'#142F40','Arial Unicode MS');foot(s,pick('完整源文件逐行保留；显示换行不改变 R 源码。请从 .R 文件运行。','Every source line is retained. Display wrapping does not alter the R source. Run the .R files.'));codeManifest.push({slide:p.slides.items.length,file,displayStart:start,displayEnd:end,sourceLines:lines.length});}
}
const count=p.slides.items.length;if(count-10>50)throw new Error('Appendix exceeds 50 slides: '+count);
const outDir=path.join(W,'qa','deck_'+lang);await fs.mkdir(outDir,{recursive:true});
await fs.writeFile(path.join(B,'manifest_'+lang+'.json'),JSON.stringify({slides:manifest,code:codeManifest,total:count,main:10,appendix:count-10,font},null,2));
const draft=path.join(B,'draft_'+lang+'_'+Date.now()+'.pptx');await (await PresentationFile.exportPptx(p)).save(draft);
console.log('Exported',lang,count,'slides');
for(let i=0;i<count;i++){const s=p.slides.items[i];const b=await p.export({slide:s,format:'png',scale:1});await fs.writeFile(path.join(outDir,`slide-${String(i+1).padStart(2,'0')}.png`),new Uint8Array(await b.arrayBuffer()));if((i+1)%10===0)console.log('Rendered',lang,i+1);}
const finalPath=path.join(W,'qa',`validated_${lang}_${Date.now()}.pptx`);
await finalizePresentation({workspaceDir:W,candidatePath:draft,finalPath,pythonExecutable:PY,integrityValidatorPath:path.join(SK,'container_tools/inspect_presentation_package_integrity.py'),layoutValidatorPath:path.join(SK,'container_tools/inspect_presentation_layout_geometry.py'),layoutArgs:['--expected-slide-size-emu','12192000,6858000','--validate-bullet-geometry','--validate-heading-fit',...[...new Set(tableSlides)].flatMap(n=>['--require-native-table-slide',String(n)])],explicitTotalSlideCount:count,requiredNativeTableOwnerSlides:[...new Set(tableSlides)],requiredNativeChartOwnerSlides:[...new Set(chartSlides)],materializeLiteralChartWorkbooks:true,fontPolicy:{basis:'design',families:[...new Set([font,'Arial','Arial Unicode MS'])],scriptFonts:{ea:'Arial Unicode MS'}},verifyArtifactToolImport:true,receiptPath:path.join(B,'validation_'+lang+'.json')});
const dest=path.join(W,'output',zh?'Assignment3_最终演示_中文版.pptx':'Assignment3_Final_Presentation_English.pptx');await fs.copyFile(finalPath,dest);console.log('FINAL',dest);
