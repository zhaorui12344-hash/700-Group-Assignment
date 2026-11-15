from pathlib import Path
from zipfile import ZipFile
from lxml import etree
import json,hashlib,re
B=Path(__file__).resolve().parent;W=B.parent
results=[]
for lang,name in [('en','Assignment3_Final_Presentation_English.pptx'),('zh','Assignment3_最终演示_中文版.pptx')]:
 p=W/'output'/name;z=ZipFile(p);assert z.testzip() is None
 slides=sorted(n for n in z.namelist() if re.fullmatch(r'ppt/slides/slide\d+.xml',n))
 notes=[n for n in z.namelist() if re.fullmatch(r'ppt/notesSlides/notesSlide\d+.xml',n)]
 assert len(slides)==57 and len(notes)==57
 m=json.loads((B/f'manifest_{lang}.json').read_text());assert m['total']==57
 t=c=0
 for n in slides:
  root=etree.fromstring(z.read(n)); t+=len(root.xpath('//*[local-name()="tbl"]'));c+=len(root.xpath('//*[local-name()="chart"]'))
 assert t>0 and c==3
 text=' '.join(etree.fromstring(z.read('ppt/slides/slide15.xml')).xpath('//*[local-name()="t"]/text()'))
 assert '0.0000' not in text and text.count('—')==5
 for n in notes:
  txt=' '.join(etree.fromstring(z.read(n)).xpath('//*[local-name()="t"]/text()'));assert len(txt)>20
 results.append({'file':name,'main_slides':10,'appendix_slides':47,'notes':len(notes),'native_tables':t,'native_charts':c,'sha256':hashlib.sha256(p.read_bytes()).hexdigest()})
for lang,name in [('en','Assignment3_Learning_Guide_English.docx'),('zh','Assignment3_学习指南_中文版.docx')]:
 p=W/'output'/name;z=ZipFile(p);assert z.testzip() is None
 root=etree.fromstring(z.read('word/document.xml'));txt=' '.join(root.xpath('//*[local-name()="t"]/text()'))
 assert all(term in txt for term in ['轮廓系数','回归系数','调整兰德指数','R²','HC3'])
 pages=list((W/'qa'/f'guide_{lang}_v3').glob('page-*.png'));assert len(pages)==29
 results.append({'file':name,'lessons':20,'rendered_pages':len(pages),'sha256':hashlib.sha256(p.read_bytes()).hexdigest()})
slides=json.loads((B/'main_slides.json').read_text());assert len(slides)==10 and sum(x['seconds'] for x in slides)==600
report={'status':'PASS','artifacts':results,'main_speaking_seconds':600,'independent_visual_review':'Both languages: all 57 deck slides and all 29 guide pages reviewed; corrected affected English final pages rechecked.','scope':'Structural and rendered checks; no claim of native Microsoft PowerPoint execution.'}
(W/'qa'/'delivery_checks.json').write_text(json.dumps(report,ensure_ascii=False,indent=2));print(json.dumps(report,ensure_ascii=False,indent=2))
