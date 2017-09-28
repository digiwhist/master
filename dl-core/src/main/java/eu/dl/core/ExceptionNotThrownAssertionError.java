package eu.dl.core;

/**
 * Exception thrown in case of assertion error.
 * 
 * @author Kuba Krafka
 */
public class ExceptionNotThrownAssertionError extends AssertionError {

    private static final long serialVersionUID = -7409019204642099924L;

    /**
     * Default constructor.
     */
    public ExceptionNotThrownAssertionError() {
        super("Expected exception was not thrown.");
    }
}