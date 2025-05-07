# toga    

sudo nixos-rebuild switch --flake /home/toga/toga#nixos

```mermaid
flowchart LR
  %% Marketing
  subgraph Marketing
    direction TB
    A[Prospect Capturado] --> B{Origem do Lead}
    B -->|Site| C[CRM Tag: Site]
    B -->|Visita ao Campus| D[CRM Tag: Evento]
    B -->|Feira de Educacao| E[CRM Tag: Feira]
    B -->|Redes Sociais| F[CRM Tag: Social]
    B -->|Indicacao| G[CRM Tag: Referral]
  end

  %% Admissoes  
  subgraph Admissoes
    direction TB
    C --> H[Lead Scoring e Segmentacao]
    D --> H
    E --> H
    F --> H
    G --> H
    H --> I{Lead Qualificado?}
    I -->|Nao| J[Campanha de Nutricao Automatica]
    J --> K[Reavaliar apos X dias]
    K --> I
    I -->|Sim| L[Atribuir a Conselheiro]
    L --> M[Contato Inicial - Email ou Telefone]
    M --> N{Interesse Confirmado?}
    N -->|Nao| O[Marcar como Frio]
    N -->|Sim| P[Agendar Tour no Campus]
    P --> Q[Envio de Informacoes de Inscricao]
    Q --> R[Candidato envia Formulario]
    R --> S[Comissao Avalia Inscricao]
    S --> T{Decisao de Admissao}
    T -->|Rejeitado| U[Enviar Carta de Regret]
    T -->|Aceito| V[Enviar Carta de Oferta]
  end

  %% Matricula  
  subgraph Matricula
    direction TB
    V --> W[Processo de Matricula e Pagamento]
    W --> X[Orientacao e Boas-vindas]
    X --> Z[Fim do Fluxo]
  end

  %% Estilos
  style A fill:#E8F1FF,stroke:#0366D6
  style Z fill:#E6FFEA,stroke:#28A745
