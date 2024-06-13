package final_rdi.rdifinal.service;
import final_rdi.rdifinal.config.AppConfig;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
import org.apache.commons.csv.CSVRecord;
import org.springframework.stereotype.Service;

import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

// Anotação para indicar que a classe é um serviço
@Service
public class DataClean {
    private static final Logger LOGGER = Logger.getLogger(DataClean.class.getName());


    // Método para limpar os dados do arquivo CSV
    public void cleanData() {
        String dataPath = AppConfig.path_base;
        String cleanedDataPath = AppConfig.path_finalbase;
        LOGGER.info("Caminho de leitura: " + dataPath);
        
        // Lê o arquivo CSV e escreve o arquivo limpo
        try {
            Reader in = new FileReader(dataPath);
            Iterable<CSVRecord> records = CSVFormat.RFC4180.withFirstRecordAsHeader().parse(in);
    
            Writer out = new FileWriter(cleanedDataPath);
            CSVPrinter printer = new CSVPrinter(out, CSVFormat.DEFAULT.withHeader((String[]) records.iterator().next().toMap().keySet().toArray(new String[0])));
    
            processRecords(records.iterator(), printer);
    
            printer.close();
            out.close();
            LOGGER.info("Caminho da base tratada: " + cleanedDataPath);
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, "Erro ao processar o CSV", e);
        }
    }

    // Método para processar os registros do arquivo CSV
    private void processRecords(Iterator<CSVRecord> iterator, CSVPrinter printer) throws IOException {
        if (iterator.hasNext()) {
            // Processa os registros
            CSVRecord record = iterator.next();
            // Verifica se a fase é "Semi-Finais"
            Map<String, String> values = record.toMap();
            // Se a fase for "Semi-Finais", imprime o registro
            if ("Semi-Finais".equals(values.get("fase"))) {
                printer.printRecord(values.values());
            }
            processRecords(iterator, printer);
        }
    }
}