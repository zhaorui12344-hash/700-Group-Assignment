from pathlib import Path
import hashlib,shutil,csv,json,collections
W=Path(__file__).resolve().parent.parent; O=W/'700_Organized'
O.mkdir(exist_ok=True); records=[]; canonical={}
def digest(p):return hashlib.sha256(p.read_bytes()).hexdigest()
def copy(src,dst):
 src=W/src if not isinstance(src,Path) else src
 if not src.is_file() or src.is_symlink():return
 sha=digest(src);rel=str(src.relative_to(W))
 if sha in canonical:
  records.append([rel,canonical[sha],'duplicate: same SHA-256',sha]);return
 target=O/dst;target.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(src,target)
 canonical[sha]=str(dst);records.append([rel,str(dst),'included',sha])
def tree(src,dst,exclude=lambda p:False):
 root=W/src
 for p in sorted(root.rglob('*')):
  if not p.is_file() or p.is_symlink():continue
  rel=p.relative_to(root)
  if any(x.startswith('.') or x=='__pycache__' for x in rel.parts) or exclude(rel):continue
  copy(p,Path(dst)/rel)
for source,dest in [('W3','W03_Univariate'),('w4','W04_Bivariate_I'),('W6','W06_Bivariate_II'),('W7','W07_Clustering'),('W8','W08_Regression')]:tree(source,'01_Course_Materials/'+dest)
for name in ['Group_presentation.csv','assignment requirement.docx']:copy('group/'+name,Path('02_Assignment/group')/name)
tree('group/analysis','02_Assignment/group/analysis',lambda p:'development_archive' in p.parts)
for name in ['Assignment3_Learning_English.R','Assignment3_Learning_Chinese.R','README.txt','VALIDATION.json']:
 copy('group/R_Learning_Edition/'+name,Path('03_R_Learning_Edition')/name)
for name in ['Assignment3_Learning_Guide_English.docx','Assignment3_学习指南_中文版.docx']:copy('group/learning_and_presentation/output/'+name,Path('04_Learning_Guides')/name)
for name in ['Assignment3_Final_Presentation_English.pptx','Assignment3_最终演示_中文版.pptx']:copy('group/learning_and_presentation/output/'+name,Path('05_Final_Presentations')/name)
copy('group/preliminary_analysis.py',Path('06_Preliminary_Analysis/preliminary_analysis.py'))
tree('group/preliminary_outputs','06_Preliminary_Analysis/preliminary_outputs')
for p in sorted((W/'group/R_Learning_Edition/build').iterdir()):
 if p.is_file() and p.suffix in ['.py','.R','.json','.log'] and p.name!='parse_check.log':copy(p,Path('07_Reproducibility_and_QA/R_learning_build')/p.name)
for p in sorted((W/'group/learning_and_presentation/build').iterdir()):
 if p.is_file() and p.suffix in ['.py','.mjs','.json','.conf'] and not p.name.startswith('probe'):
  copy(p,Path('07_Reproducibility_and_QA/presentation_build')/p.name)
copy('group/learning_and_presentation/qa/delivery_checks.json',Path('07_Reproducibility_and_QA/delivery_checks.json'))
copy('group/learning_and_presentation/WORK_PLAN.md',Path('07_Reproducibility_and_QA/presentation_work_plan.md'))
copy('group/organize_for_github.py',Path('07_Reproducibility_and_QA/organize_for_github.py'))
# Explicit inventory of omitted packaging/working files; all original course materials are retained.
exclusions=[('group/Assignment3/','Earlier packaged copy; current analysis is authoritative.'),('group/Assignment3_R_完整分析.zip','Duplicate package of already classified analysis/source data.'),('group/analysis/logs/development_archive/','Temporary development workspaces and exploratory console artifacts.'),('group/learning_and_presentation/qa/','Rendered preview caches, intermediate PDFs and validation copies; final checks retained separately.'),('group/learning_and_presentation/build/draft_*','Intermediate PPT drafts; only validated final PPTs retained.'),('group/R_Learning_Edition/runs/','Recreated outputs duplicate the authoritative 81 CSVs; validation retained.'),('group/R_Learning_Edition/data/','Identical dataset reused from 02_Assignment/group.'),('group/R_Learning_Edition/reference_original/','Identical original code reused from 02_Assignment/group/analysis.'),('node_modules / __pycache__ / .DS_Store','Runtime dependencies and machine caches are not coursework.')]
for a,b in exclusions:records.append([a,'','excluded: '+b,''])
meta=O/'07_Reproducibility_and_QA';meta.mkdir(exist_ok=True)
with (meta/'ORGANIZATION_LOG.csv').open('w',newline='',encoding='utf-8-sig') as f:
 wr=csv.writer(f);wr.writerow(['original_path','repository_path','decision','sha256']);wr.writerows(records)
(O/'.gitignore').write_text('.DS_Store\n.Rhistory\n.RData\n__pycache__/\nnode_modules/\n03_R_Learning_Edition/runs/\n')
print(json.dumps({'included':sum(r[2]=='included' for r in records),'exact_duplicate_files_skipped':sum(r[2].startswith('duplicate') for r in records),'bytes':sum((O/r[1]).stat().st_size for r in records if r[2]=='included')},indent=2))
