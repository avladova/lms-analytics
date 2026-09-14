import json
from pathlib import Path
p=Path('/home/ubuntu/.manus/config/config.json')
data=json.loads(p.read_text())
items=data.get('connectors', data if isinstance(data,list) else [])
for item in items:
    if item.get('name') == 'GitHub':
        item['enabled'] = True
        break
else:
    raise SystemExit('GitHub connector not found')
p.write_text(json.dumps(data, ensure_ascii=False, indent=2)+'\n')
print('GitHub connector enabled in draft config')
