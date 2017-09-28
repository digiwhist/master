package eu.dl.worker.runner;

import java.util.Arrays;

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
        // validate input params
        if (args.length != 2) {
            logger.error("This class accepts exactly two arguments. First one is fully qualified worker name"
                + " (implementing Worker interface) and second is configuration name (file name without '.properties'"
                + " extension).");
            System.exit(1);
        }
        final String workerName = args[0];

        final String configName = args[1];

        Config.getInstance().setConfigFile(Arrays.asList(configName));

        // get the worker via reflection
        try {
            @SuppressWarnings("rawtypes")
            final Class workerClass = Class.forName(workerName);
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
