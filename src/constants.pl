% Módulo constants.pl - Constantes do Sistema
%
% Responsável por:
% - Definir parâmetros constantes do sistema
% - Gerenciar códigos de cores ANSI para a interface
% - Centralizar configurações de dimensões da UI
% - Padronizar esquema de cores em todo o sistema
:- module(constants, [
    ui_width/1,         % Largura da interface
    title_color/1,      % Cor para títulos
    option_color/1,     % Cor para opções
    success_color/1,    % Cor para mensagens de sucesso
    warning_color/1,    % Cor para avisos
    error_color/1,      % Cor para mensagens de erro
    reset_color/1,      % Código para resetar cores
    blue_color/1,       % Cor azul para destaque
    magenta_color/1     % Cor magenta para destaque
]).

% =============================================
% CONFIGURAÇÕES DE INTERFACE
% =============================================

%% ui_width(-Width)
% Define a largura padrão da interface do usuário
%
% Valor: 
%   Width - Número inteiro representando a largura em caracteres
%
% Uso:
%   Utilizado para centralizar e alinhar elementos da UI
ui_width(50).

% =============================================
% CÓDIGOS DE CORES ANSI
% =============================================

%% title_color(-ColorCode)
% Código ANSI para cor de títulos (Ciano brilhante)
title_color("\033[1;36m").     % Ciano brilhante
option_color("\033[0;37m").    % Branco
success_color("\033[1;32m").   % Verde brilhante
warning_color("\033[1;33m").   % Amarelo/Laranja brilhante
error_color("\033[1;31m").     % Vermelho brilhante
reset_color("\033[0m").        % Reset
blue_color("\033[1;34m").      % Azul brilhante
magenta_color("\033[1;35m").   % Magenta brilhante
