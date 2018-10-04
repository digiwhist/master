package eu.dl.worker.runner;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.core.config.Config;
import eu.dl.worker.Worker;

/**
 * This is runner for our workers. Should be used from CLI.
 *
 * @author Kuba Krafka
 */
final class Runner {

    /**
     * Logger.
     */
    private static final Logger logger = LoggerFactory.getLogger(Runner.class);

    /**
     * This class shouldn't be instantiated.
     */
    private Runner() {
        // dont instantiate this class
    }

    /**
     * Entry point.
     * @param args argument specifying worker name
     */
    public static void main(final String[] args) {
        final List<String> workerNames = new ArrayList<>();

        String configName = "";

        for (String arg : args) {
            if (arg.startsWith("eu.")) {
                //worker
                workerNames.add(arg);
            } else {
                configName = arg;
            }
        }

        if (configName.isEmpty()) {
            logger.error("No configuration name provided");
            System.exit(1);
        }

        Config.getInstance().addConfigFile(configName);


        for (String workerName : workerNames) {
            // get the worker via reflection
            try {
                @SuppressWarnings("rawtypes") final Class workerClass = Class.forName(workerName);
                final Worker worker = (Worker) workerClass.newInstance();
                worker.startWork();
                logger.info("Starting worker {}", workerName);
            } catch (final ClassNotFoundException e) {
                logger.error("No such worker found '{}' exception {}", workerName, e.toString());
                e.printStackTrace();
                System.exit(1);
            } catch (final InstantiationException e) {
                logger.error("Unable to instantiate the worker '{}' exception {}", workerName, e.toString());
                e.printStackTrace();
                System.exit(1);
            } catch (final IllegalAccessException e) {
                logger.error("Unable to access the worker '{}' exception {}", workerName, e.toString());
                e.printStackTrace();
                System.exit(1);
            }
        }
    }
}
