const express = require('express');
const { spawn } = require('child_process');
const path = require('path');
const app = express();
const cors = require('cors');

app.use(express.json());
app.use(cors());

app.post('/api/sortear', (req, res) => {
    // Pega os dados que vieram do Frontend
    // O JSON deve ter: { "qtdTimes": 2, "maxPorTime": 5, "jogadores": [...] }
    const inputData = req.body;

    console.log("Recebendo pedido de sorteio...");

   
    const haskellProcess = spawn('./sorteador', []);

    let outputData = '';
    let errorData = '';

    // Envia o JSON para o Haskell (stdin)
    haskellProcess.stdin.write(JSON.stringify(inputData));
    haskellProcess.stdin.end();

 
    haskellProcess.stdout.on('data', (data) => {
        outputData += data.toString();
    });

   
    haskellProcess.stderr.on('data', (data) => {
        errorData += data.toString();
        console.error(`Erro Haskell: ${data}`);
    });

    
    haskellProcess.on('close', (code) => {
        if (code !== 0) {
            return res.status(500).json({ erro: "Erro ao executar Haskell", detalhe: errorData });
        }

        try {
            
            const result = JSON.parse(outputData);
            res.json(result);
        } catch (e) {
            console.error("Erro ao parsear JSON do Haskell:", outputData);
            res.status(500).json({ erro: "Resposta inválida do gerador" });
        }
    });
});

const PORT = 3001;
app.listen(PORT, () => {
    console.log(`Servidor rodando em http://localhost:${PORT}`);
});


