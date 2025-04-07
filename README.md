# 🎨 Nonograma

Bem-vindo ao **Nonograma**! Um jogo de quebra-cabeça lógico-visual onde você deve preencher a grade corretamente para revelar uma imagem oculta! 🖼️

---

## 🚀 Funcionalidades
✅ Escolha de fases com **três dificuldades** (fácil, média e difícil) 🏆

✅ **Salvar** e **carregar** o progresso do jogo 💾

✅ Navegação pelo tabuleiro via **WASD** 🎮

✅ **Marcação** de células como coloridas ou "X" ✏️

✅ **Sistema de pistas** para ajudar 🎁

✅ **Vidas limitadas**, cada erro custa uma ❤️

✅ **Game Over** se todas as vidas acabarem ❌

---

## 🔧 Requisitos
🔹 [SWI - Prolog](https://www.swi-prolog.org/download/stable) ⚙️

---

## 🎯 Como Executar
1️⃣  Clone este repositório:
   ```sh
   git clone https://github.com/kemilli-lima/nonograma-prolog-plp.git
   ```

2️⃣ Para garantir os efeitos visuais e as cores do jogo no terminal, é necessário executar os seguintes comandos antes:
   ```sh
   # Windows
   [Console]::OutputEncoding = [System.Text.Encoding]::UTF8
   chcp 65001
   ```
   ```sh
   # Linux
   export LANG=en_US.UTF-8
   export LC_ALL=en_US.UTF-8
   locale # checar se funcionou
   ```
   ```sh
   # Mac
   defaults write -g AppleLocale en_US.UTF-8
   ```

3️⃣  Compile e execute o jogo dentro do diretório raíz:
   ```sh
   swipl -g run -o nonograma -c src/main.pl
   ```

---

## 🎮 Como Jogar
🎯 Inicie um novo jogo e escolha o **nível de dificuldade** ou carregue um **jogo salvo**

🎯 Use as teclas **WASD** para navegar pela grade

🎯 Pressione a tecla **f** para marcar uma célula como preenchida ou a tecla **m** para não-preenchida

🎯 Pressione a tecla **h** para pedir uma pista 💡

🎯 É possível **salvar** seu progresso, apertando a tecla **v** e digitando o nome do arquivo .sav desejado (Apenas o nome do arquivo)

🎯 Caso deseje recuperar um jogo salvo, selecione o **número correspondente** no menu. 

🎯 O jogo termina quando você **completa o quebra-cabeça** ou **perde todas as vidas**

---

## 🤝 Contribuição
O jogo foi desenvolvido para a disciplina de Paradigmas de Linguagem de Programação do curso de Ciência da Computação na Universidade Federal de Campina Grande! Os alunos responsáveis pelo projeto são:
- Kemilli Lima ([Github](https://github.com/kemilli-lima))
- Júlia Leal ([Github](https://github.com/julia-dsleal))
- João Victor Limeira ([Github](https://github.com/joaoSilvaL)) 
- Carlos Nogueira ([Github](https://github.com/carlsnog)) 
- Nívea Calébia ([Github](https://github.com/calebia))  

---

## 📜 Licença
Este projeto está licenciado sob a [MIT License](LICENSE). 