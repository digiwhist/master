package eu.dl.worker;

/**
 * Basic functional piece of the whole framework. It encapsulates functionality
 * for one of the data handling phases such as crawling, data downloading
 * parsing etc.
 *
 * @author Kuba Krafka
 */
public interface Worker {
    /**
     * Starts the work - connects to queues etc. and performs work delivered.
     *
     */
    void startWork();
}
