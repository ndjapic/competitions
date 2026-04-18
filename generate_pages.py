import os

# Путања до твог кода и где желиш странице
src_dir = 'docs/src'
output_dir = 'docs/zadaci'

# Направи фолдер за странице ако не постоји
if not os.path.exists(output_dir):
    os.makedirs(output_dir)

for root, dirs, files in os.walk(src_dir):
    for file in files:
        if file.endswith('.pas'):
            # Путања до оригиналног фајла
            file_path = os.path.join(root, file)
            
            # Направи име за .md фајл
            relative_path = os.path.relpath(root, src_dir)
            target_folder = os.path.join(output_dir, relative_path)
            
            if not os.path.exists(target_folder):
                os.makedirs(target_folder)
            
            md_file_name = file.replace('.pas', '.md')
            md_file_path = os.path.join(target_folder, md_file_name)
            
            # Прочитај паскал код
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                pascal_code = f.read()
            
            # Напиши маркдаун фајл
            with open(md_file_path, 'w', encoding='utf-8') as f:
                f.write(f"# Задатак: {file}\n\n")
                f.write("```pascal\n")
                f.write(pascal_code)
                f.write("\n```\n")

print("Готово! Све странице су генерисане у docs/zadaci")
