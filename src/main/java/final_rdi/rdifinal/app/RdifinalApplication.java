package final_rdi.rdifinal.app;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@SpringBootApplication
public class RdifinalApplication {

	private static final Logger logger = LoggerFactory.getLogger(RdifinalApplication.class);

	public static void main(String[] args) {
		SpringApplication.run(RdifinalApplication.class, args);
		logger.info("Testando app");
	}

}
