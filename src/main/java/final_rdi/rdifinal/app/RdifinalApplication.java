package final_rdi.rdifinal.app;

import java.util.logging.Logger;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.ConfigurableApplicationContext;

import final_rdi.rdifinal.config.AppConfig;
import final_rdi.rdifinal.service.DataClean;
// Anotação para indicar que a classe é uma aplicação Spring Boot
@SpringBootApplication
// Anotação para indicar que a classe deve escanear os pacotes indicados
@ComponentScan(basePackages = {"final_rdi.rdifinal.app", "final_rdi.rdifinal.service"})
public class RdifinalApplication {
	private static final Logger LOGGER = Logger.getLogger(DataClean.class.getName());


    public static void main(String[] args) {
        ConfigurableApplicationContext context = SpringApplication.run(RdifinalApplication.class, args);
        DataClean dataClean = context.getBean(DataClean.class);
        dataClean.cleanData();
		LOGGER.info("Base de dados limpa. Arquivos gerados em: D:\\CDMI\\rdifinal\\src\\main\\resources\\data");
    }
}