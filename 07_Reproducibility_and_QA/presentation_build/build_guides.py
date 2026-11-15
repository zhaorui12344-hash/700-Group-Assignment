import json,csv,re
from pathlib import Path
from docx import Document
from docx.shared import Inches,Pt,RGBColor
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.enum.table import WD_TABLE_ALIGNMENT,WD_CELL_VERTICAL_ALIGNMENT
B=Path(__file__).resolve().parent; ROOT=B.parent; A=ROOT.parent/'analysis'
L=json.loads((B/'lessons.json').read_text())
P=list(csv.DictReader((A/'tables/02_cluster_profiles.csv').open()))
refs=[('W3','Week 3 - Univariate analysis 2026 (Class).pptx','7–11, 15, 20–23, 26, 31, 33–35'),('W4','Week 4 - Bivariate analysis 1.pptx','3–9, 11–17, 21–23, 25–27'),('W6','Week 6 - Bivariate analysis 2.pptx','9–16, 19–24'),('W7','Week 7 - Cluster analysis.pptx','11–21, 24–30'),('W8','Week 8 - Linear Regression.pptx','3, 20–30, 32–33')]
G=[('Mean','均值','Sum divided by count. Sensitive to extreme magnitudes.','总和除以数量，容易受极端值影响。'),('Median','中位数','Middle ordered value. Useful for skewed outcomes.','排序的中间值，适合辅助描述偏斜分布。'),('Standard deviation','标准差','Spread in the original units.','用原变量单位衡量分散程度。'),('IQR','四分位距','Q3 minus Q1. Describes the middle 50%.','Q3 减 Q1，描述中间 50% 的跨度。'),('z-score','标准分数','Distance from the mean in standard-deviation units.','某值离均值几个标准差。'),('Correlation r','相关系数','Direction and strength of association.','关联方向与强度，不能直接推断因果。'),('p-value','p 值','Tail probability under the null and assumptions.','在零假设与前提成立时的极端统计量概率。'),('Confidence interval','置信区间','Uncertainty for an estimated parameter.','对估计参数不确定性的区间表达。'),('Chi-square','卡方','Compares observed with expected category counts.','比较类别观察人数与独立假设下期望人数。'),('ANOVA','方差分析','Tests equality of several group means.','检验多个组的均值是否相同。'),('Centroid','聚类中心','Mean position of a cluster on its input variables.','群体在聚类输入变量上的平均位置。'),('Silhouette','轮廓系数','Compares within-group with nearest-other-group distances.','比较群内距离与最近其他群的距离。'),('Coefficient','回归系数','Conditional change in a model prediction per unit.','其他已纳入变量相同时的单位预测变化。'),('Residual','残差','Observed minus fitted value.','实际值减拟合值。'),('R²','决定系数','One minus squared error relative to mean-reference variation.','1 减模型平方误差占均值参照变异的比例。'),('MAE','平均绝对误差','Mean absolute prediction error, in outcome units.','绝对预测误差的平均值，单位与结果相同。'),('RMSE','均方根误差','Square root of mean squared error.','平方误差平均后开平方，更重视大误差。'),('VIF','方差膨胀因子','Redundancy among regression predictors.','检查回归预测变量之间的重复信息。'),('HC3','稳健标准误','Coefficient uncertainty allowing unequal error variance.','允许误差方差不齐的系数不确定性估计。'),('ARI','调整兰德指数','Agreement between two sets of cluster assignments.','比较两次分组的一致程度。')]
def setfont(style,name,size):
 style.font.name=name;style.font.size=Pt(size);style.font.color.rgb=RGBColor(0,0,0)
 rpr=style.element.get_or_add_rPr();rf=rpr.find(qn('w:rFonts'))
 if rf is None:rf=OxmlElement('w:rFonts');rpr.insert(0,rf)
 for k in ('ascii','hAnsi','eastAsia','cs'):rf.set(qn('w:'+k),name)
def table(d,headers,rows,widths):
 t=d.add_table(rows=1,cols=len(headers));t.alignment=WD_TABLE_ALIGNMENT.CENTER;t.autofit=False
 for i,(h,w) in enumerate(zip(headers,widths)):t.columns[i].width=Inches(w);t.rows[0].cells[i].text=h
 for row in rows:
  for cell,txt in zip(t.add_row().cells,row):cell.text=str(txt)
 for ri,row in enumerate(t.rows):
  for ci,c in enumerate(row.cells):
   c.width=Inches(widths[ci]);c.vertical_alignment=WD_CELL_VERTICAL_ALIGNMENT.CENTER
   pr=c._tc.get_or_add_tcPr();sh=OxmlElement('w:shd');sh.set(qn('w:fill'),'DCEAF2' if ri==0 else ('F4F7F9' if ri%2==0 else 'FFFFFF'));pr.append(sh)
   borders=OxmlElement('w:tcBorders')
   for side in ('top','left','bottom','right'):
    e=OxmlElement('w:'+side);e.set(qn('w:val'),'single');e.set(qn('w:sz'),'4');e.set(qn('w:color'),'D9D9D9');borders.append(e)
   pr.append(borders)
   for p in c.paragraphs:
    p.paragraph_format.space_after=Pt(5);p.paragraph_format.space_before=Pt(5)
    for run in p.runs:
     run.font.size=Pt(9.5);run.bold=ri==0
     if re.search('[\u4e00-\u9fff]',run.text):
      run.font.name='Arial Unicode MS'
      rf=run._element.get_or_add_rPr().rFonts
      for key in ('ascii','hAnsi','eastAsia','cs'):rf.set(qn('w:'+key),'Arial Unicode MS')
  trpr=row._tr.get_or_add_trPr();trpr.append(OxmlElement('w:cantSplit'))
 t.rows[0]._tr.get_or_add_trPr().append(OxmlElement('w:tblHeader'))
 return t
def page(d,title):
 d.add_page_break();d.add_heading(title,1)
def main(lang):
 zh=lang=='zh';d=Document();s=d.sections[0];s.page_width=Inches(8.27);s.page_height=Inches(11.69)
 s.top_margin=Inches(.72);s.bottom_margin=Inches(.7);s.left_margin=s.right_margin=Inches(.78)
 f='Arial Unicode MS' if zh else 'Arial'
 for name,size in [('Normal',11),('Title',28),('Subtitle',15),('Heading 1',19),('Heading 2',12),('Caption',9)]:setfont(d.styles[name],f,size)
 d.styles['Normal'].paragraph_format.line_spacing=1.15;d.styles['Normal'].paragraph_format.space_after=Pt(8)
 d.styles['Heading 1'].paragraph_format.space_after=Pt(15)
 d.styles['Heading 2'].paragraph_format.space_before=Pt(10);d.styles['Heading 2'].paragraph_format.space_after=Pt(5)
 footer=s.footer.paragraphs[0];footer.alignment=WD_ALIGN_PARAGRAPH.RIGHT
 r=footer.add_run();fld=OxmlElement('w:fldSimple');fld.set(qn('w:instr'),'PAGE');r._r.addnext(fld)
 d.core_properties.title='Assignment 3 零基础学习指南' if zh else 'Assignment 3 Beginner Learning Guide'
 d.core_properties.subject='Customer segmentation and profitability analysis';d.core_properties.author=''
 d.add_paragraph('Assignment 3',style='Subtitle')
 d.add_paragraph('客户细分与盈利分析\n零基础学习指南' if zh else 'Customer Segmentation\nand Profitability Analysis\nA Beginner Learning Guide',style='Title')
 d.add_paragraph('中文版' if zh else 'English edition',style='Subtitle')
 d.add_paragraph('这份指南教你理解已完成的分析：为什么做每一步，数字如何得到，结果如何解释，以及哪些结论不能从数据推出。你不需要先掌握统计学，也不需要先会写 R。' if zh else 'This guide teaches the completed analysis step by step: why each method is used, how to read its numbers, and what the evidence can and cannot establish. No prior statistics or R programming is required.')
 d.add_paragraph('建议按顺序阅读，每次学习 2 至 3 课。遇到公式先读文字，再尝试手算小例子。每课末尾都有自测与答案。' if zh else 'Read in order, two or three lessons at a time. Read the explanation before the formula, then try the small worked examples. Each lesson ends with a question and answer.')
 d.add_heading('先记住的结论' if zh else 'The conclusions to keep in mind',2)
 d.add_paragraph('数据形成了六个有重叠的客户群。主模型可以解释一部分记录 CLV 的差异，但无法验证真实未来价值。当前利润模型较弱，因此业务建议以核实数据和小规模试点为主。' if zh else 'Six overlapping customer groups provide useful profiles. The main model explains part of the variation in recorded CLV but cannot validate true future value. Current-profit prediction is weak, so recommendations focus on data checks and controlled pilots.')
 d.add_paragraph('依据：10,000 条客户记录、Assignment 3 要求和 W3、W4、W6、W7、W8 课堂材料。教学例子与实际结果在正文中明确区分。' if zh else 'Based on 10,000 customer records, the Assignment 3 brief and W3, W4, W6, W7 and W8 materials. Teaching examples are explicitly separated from actual results.')
 page(d,'学习路线' if zh else 'Learning route')
 for i,l in enumerate(L,1):d.add_paragraph(f'{i:02d}  '+l['title'][lang])
 d.add_paragraph('补充材料：六群速查表、术语表、读图练习、课堂与结果来源。' if zh else 'Reference material: cluster summary, glossary, guided figure reading, and source map.')
 for i,l in enumerate(L,1):
  # Keep one compact lesson per page, with natural overflow if needed.
  page(d,f'{i:02d}  '+re.sub('[：:？?]',' ',l['title'][lang]))
  for b in l['blocks']:
   d.add_heading(b['label'][lang],2);d.add_paragraph(b['text'][lang])
  p=d.add_paragraph(('回看来源：' if zh else 'Revisit: ')+l['source']);p.style=d.styles['Caption']
 page(d,'六群结果速查' if zh else 'Six-cluster reference')
 names_en=['Younger single-product low activity','Multi-product moderate activity','Single-product high activity','Multi-product high activity','High recorded income and activity','Older mainly single-product']
 rows=[]
 for i,r in enumerate(P):rows.append([f'C{i+1}',r['name'] if zh else names_en[i],r['n'],f"{float(r['share'])*100:.2f}%",f"{float(r['Yield_mean']):.0f}",f"{float(r['CLV_mean']):.0f}"])
 table(d,['群' if zh else 'ID','描述' if zh else 'Profile','人数' if zh else 'n','占比' if zh else 'Share','均值 Yield $' if zh else 'Mean Yield $','均值 CLV $' if zh else 'Mean CLV $'],rows,[.45,2.4,.6,.7,1,1])
 d.add_paragraph('金额已四舍五入。每个群的画像均使用未截尾的原始金额。C1 与 C3 的 Yield 中位数均为 −$25，提示均值不能代表多数客户。' if zh else 'Dollar values are rounded. Profiles use original uncapped amounts. Median Yield is −$25 in both C1 and C3, reminding us that means need not describe the majority.')
 d.add_heading('年龄类别并不是数值年龄' if zh else 'Age codes are not numeric ages',2)
 table(d,['Cat_Age','年龄范围' if zh else 'Observed age range'],[['2','18–35'],['3','36–50'],['4','51–64'],['5','65–79'],['6','80–99']],[1.5,4.65])
 d.add_paragraph('本样本没有 Cat_Age=1。为了专题比较，另划分 18–29 和 30–35 岁等年龄带；这是分析用分组，与原始 Cat_Age 不完全相同。' if zh else 'Cat_Age=1 is absent. The analysis creates additional age bands, including 18–29 and 30–35, for the under-30 question. These are distinct from the original category codes.')
 for start in (0,10):
  page(d,('统计术语速查 ' if zh else 'Statistics glossary ')+str(start//10+1))
  table(d,['英文' if zh else 'Term','中文' if zh else 'Chinese','含义' if zh else 'Meaning'],[[g[0],g[1],g[3] if zh else g[2]] for g in G[start:start+10]],[1.45,1.15,3.55])
 figs=[('03_age_clv.png','读图 年龄与记录 CLV','Reading age and recorded CLV',
 '左图横轴是年龄，纵轴是记录 CLV；橙线显示各年龄的平均水平。纵轴只展示到第 99 百分位，部分高值不在画面中，但分析没有删除它们。高龄端人数较少，均值波动会更明显。右图每个箱体表示一个年龄组：粗线是中位数，箱体覆盖中间一半记录；图上隐藏异常点以便读图。这些描述帮助发现形状，不能证明年龄的因果效应。',
 'The left plot places age on the horizontal axis and recorded CLV on the vertical axis. The orange line shows age-specific means. The vertical view stops at the 99th percentile, although analyses retain all observations. Sparse older ages produce less stable means. On the right, each box contains the middle half of an age band and the thick line is its median. Outlier markers are hidden for readability. These descriptions reveal shape, not causal age effects.'),
 ('09_regression_diagnostics.png','读图 回归诊断四联图','Reading the four regression diagnostics',
 '先找偏离整体的点，再看整体形状。右上 Q–Q 图的上端远离参考直线，说明残差长尾明显。左下图的波动随拟合值改变，说明等方差假设值得怀疑。图中的数字是用于追踪的观测标签，不是客户真实 ID。异常记录需要核查，不能只因为图上突出就自动删除。',
 'First locate departures from the overall pattern, then examine the shape. The upper end of the Q–Q plot departs strongly from the reference line, showing a long residual tail. Changing spread in the Scale–Location plot questions equal error variance. Numeric labels trace observations; they are not real customer IDs. Prominent observations merit investigation rather than automatic removal.'),
 ('10_test_predictions.png','读图 预测与实际是否接近','Reading predictions against observations',
 '横轴是实际 CLV，纵轴是预测 CLV。虚线代表预测等于实际；点在下方表示低预测，在上方表示高预测。左图保留测试集全范围，能看到非常高的 CLV 没有被充分预测。右图放大到第 99 百分位，便于看大多数点。评价指标始终使用全体测试记录，不能只用放大后的漂亮区域计算成绩。',
 'The horizontal axis shows observed CLV and the vertical axis predicted CLV. The diagonal marks equality. Below it means underprediction; above it means overprediction. The full-range panel reveals severe underprediction for some extreme CLV records. The zoomed panel shows the bulk more clearly. All test records remain in the evaluation metrics, including those outside the zoom.')]
 for fn,tz,te,pz,pe in figs:
  page(d,tz if zh else te);d.add_picture(str(A/'plots'/fn),width=Inches(6.5));d.add_paragraph(pz if zh else pe)
  d.add_paragraph(('对应原始图：' if zh else 'Original plot: ')+fn,style='Caption')
 page(d,'课堂与分析来源' if zh else 'Classroom and analysis sources')
 d.add_paragraph('课堂引用按 PPT 文件中的实际幻灯片顺序计数，包含封面。下面列出本指南重点使用的页码。若课件使用简化定义，本指南给出更精确的解释，例如 p 值不是 H0 为真的概率，测试 R² 可以为负。' if zh else 'Classroom references use slide order in the actual PPTX, including the cover. The main locations appear below. Where slides use shorthand, this guide supplies a precise qualification, such as p-values not being probabilities of H0 and test R² potentially being negative.')
 for week,file,pages in refs:
  d.add_heading(week,2);d.add_paragraph(file+'\n'+('幻灯片 ' if zh else 'Slides ')+pages)
 d.add_heading('补充方法的用途' if zh else 'Purpose of extensions',2)
 d.add_paragraph('Spearman 应对极端值对相关的影响；Welch 与 Holm 应对方差不齐和多重比较；交叉验证和测试集检查样本外表现；HC3 修正系数不确定性的估计；ARI 比较分群稳定性。这些补充不能代替真实未来数据。' if zh else 'Spearman addresses extreme-magnitude sensitivity, Welch and Holm address unequal variances and multiple comparisons, validation checks generalisation, HC3 adjusts coefficient uncertainty, and ARI compares cluster stability. None replaces real future outcomes.')
 d.add_paragraph('完整数字与代码：group/analysis/analysis_report.html、tables 文件夹、R 文件夹、run_analysis.R。每课末尾给出具体结果表名。' if zh else 'Full results and code: group/analysis/analysis_report.html, the tables and R folders, and run_analysis.R. Each lesson identifies its relevant result tables.')
 out=ROOT/'output'/('Assignment3_学习指南_中文版.docx' if zh else 'Assignment3_Learning_Guide_English.docx');d.save(out);print(out)
for lang in ('zh','en'):main(lang)
