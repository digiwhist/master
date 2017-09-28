package eu.dl.core;

/**
 * Functional interface used to catch exceptions in jUnit.
 * 
 * @author Kuba Krafka
 */
@FunctionalInterface
public interface ExceptionThrower {

    /**
     * Throws exception.
     * 
     * @throws Throwable
     *             thrown exception
     */
    void throwException() throws Throwable;
}
