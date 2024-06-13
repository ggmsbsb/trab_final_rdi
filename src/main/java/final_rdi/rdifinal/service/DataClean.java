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
        LOGGER.info("Caminho de leitura: " + dataPath);
    
        // Caminho dos arquivos,
        String finalDataPath = AppConfig.path_finalbase;
        String semiFinalDataPath = AppConfig.path_semifinalbase;
        String quarterFinalDataPath = AppConfig.path_quarterbase;
        String octaFinalDataPath = AppConfig.path_octabase;
    
        // Lê o arquivo CSV e escreve o arquivo limpo
        try {
            Reader in = new FileReader(dataPath);
            Iterable<CSVRecord> records = CSVFormat.RFC4180.withFirstRecordAsHeader().parse(in);
    
            //Cria os arquivos de saída
            Writer outFinal = new FileWriter(finalDataPath);
            CSVPrinter printerFinal = new CSVPrinter(outFinal, CSVFormat.DEFAULT.withHeader((String[]) records.iterator().next().toMap().keySet().toArray(new String[0])));
    
            Writer outSemiFinal = new FileWriter(semiFinalDataPath);
            CSVPrinter printerSemiFinal = new CSVPrinter(outSemiFinal, CSVFormat.DEFAULT.withHeader((String[]) records.iterator().next().toMap().keySet().toArray(new String[0])));
    
            Writer outQuarterFinal = new FileWriter(quarterFinalDataPath);
            CSVPrinter printerQuarterFinal = new CSVPrinter(outQuarterFinal, CSVFormat.DEFAULT.withHeader((String[]) records.iterator().next().toMap().keySet().toArray(new String[0])));
    
            Writer outOctaFinal = new FileWriter(octaFinalDataPath);
            CSVPrinter printerOctaFinal = new CSVPrinter(outOctaFinal, CSVFormat.DEFAULT.withHeader((String[]) records.iterator().next().toMap().keySet().toArray(new String[0])));
    
            processRecords(records.iterator(), printerFinal, printerSemiFinal, printerQuarterFinal, printerOctaFinal);
    
            // Fecha os arquivos de saída
            printerFinal.close();
            printerSemiFinal.close();
            printerQuarterFinal.close();
            printerOctaFinal.close();
        } catch (IOException e) {
            LOGGER.severe("Error processing CSV file: " + e.getMessage());
        }
    }

    // Método para processar os registros do arquivo CSV
    private void processRecords(Iterator<CSVRecord> iterator, 
                            CSVPrinter printerFinal, 
                            CSVPrinter printerSemiFinal, 
                            CSVPrinter printerQuarterFinal, 
                            CSVPrinter printerOctaFinal) throws IOException {
    if (iterator.hasNext()) {
        // Processa os registros
        CSVRecord record = iterator.next();
        // Verifica se a fase é "Semi-Finais", "Final", "Quartas De Final", "Oitavas De Final"
        Map<String, String> values = record.toMap();
        String fase = values.get("fase");

        // Depending on the fase, print to the corresponding file
        if ("Final".equals(fase)) {
            printerFinal.printRecord(values.values());
        } else if ("Semi-Finais".equals(fase)) {
            printerSemiFinal.printRecord(values.values());
        } else if ("Quartas De Final".equals(fase)) {
            printerQuarterFinal.printRecord(values.values());
        } else if ("Oitavas De Final".equals(fase)) {
            printerOctaFinal.printRecord(values.values());
        }

        processRecords(iterator, printerFinal, printerSemiFinal, printerQuarterFinal, printerOctaFinal);
    }
}
}