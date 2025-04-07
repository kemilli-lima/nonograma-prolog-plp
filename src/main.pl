% Arquivo main.pl - Ponto de Entrada da Aplicação
%
% Responsável por:
% - Configurações iniciais do ambiente Prolog
% - Carregamento do controlador principal
% - Inicialização da aplicação

% =============================================
% CONFIGURAÇÕES INICIAIS
% =============================================

%% Configura encoding padrão para UTF-8
% Garante compatibilidade com caracteres especiais
:- set_prolog_flag(encoding, utf8).

% =============================================
% CARREGAMENTO DE DEPENDÊNCIAS
% =============================================

%% Carrega o módulo controller diretamente
% Diferença em relação a use_module:
% - Carrega o arquivo imediatamente
% - Não requer qualificação de módulo para predicados
:- [controller].

% =============================================
% INICIALIZAÇÃO DA APLICAÇÃO
% =============================================

%% Ponto de entrada durante inicialização
% Executado automaticamente quando o programa é carregado
:- initialization(run).

%% run
% Predicado principal que inicia a aplicação
%
% Comportamento:
% - Chama diretamente start_application do controller
% - Não requer qualificação de módulo devido ao carregamento direto
run :-
    start_application.  % Inicia o fluxo principal da aplicação
