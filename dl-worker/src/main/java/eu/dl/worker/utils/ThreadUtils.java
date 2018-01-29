package eu.dl.worker.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDateTime;
import java.util.Random;

/**
 *
 */
public final class ThreadUtils {
    private static final Logger logger = LoggerFactory.getLogger(ThreadUtils.class.getName());

    /**
     * Suppress default constructor for noninstantiability.
     */
    private ThreadUtils() {
    }

    /**
     * Sleeps for random time. The total length is randomized from interval
     * 100-(milliseconds+100). Out of working hours(20-7) sleeps for half amount
     * of time.
     *
     * @param milliseconds
     *            max time for how long the thread will sleep
     */
    public static void humanize(final Integer milliseconds) {

        try {
            final Integer currentHour = LocalDateTime.now().getHour();
            Integer sleepRange = milliseconds;
            if (currentHour > 20 || currentHour < 7) {
                // for non productive hours shorten the sleeping time
                sleepRange = milliseconds / 2;
            }
            final Random rand = new Random();
            final Integer sleepLength = rand.nextInt(sleepRange) + 100;
            logger.debug("Sleeping for randomised {}ms", sleepRange);
            Thread.sleep(sleepLength);
            logger.debug("Awake again, lets continue to work");
        } catch (final InterruptedException ex) {
            logger.debug("Thread interrupted, waiking up {}", ex);
        }
    }

    /**
     * Sleeps for defined amount of time.
     *
     * @param milliseconds
     *         max time for how long the thread will sleep
     */
    public static void sleep(final Integer milliseconds) {
        try {
            logger.debug("Sleeping for {}ms", milliseconds);
            Thread.sleep(milliseconds);
            logger.debug("Awake again, lets continue to work");
        } catch (final InterruptedException ex) {
            logger.debug("Thread interrupted, waking up {}", ex);
        }
    }
}
