package eu.dl.core;

/**
 * This exception(or its subclasses) should be thrown in case the worker failed 
 * and message shouldnt be resend, as its understood as unrecoverable. 
 * 
 * @author Kuba Krafka
 */
public class UnrecoverableException extends RuntimeException {
    
    private static final long serialVersionUID = 7991271832582449352L;

    /**
     * This exception should always provide a message.
     */
    @SuppressWarnings("unused")
    private UnrecoverableException() {
        
    }
    
    /**
     * Initialize exception with a reason for failure.
     * 
     * @param message reason for exception 
     */
    public UnrecoverableException(final String message) {
        super(message);
    }
    
    /**
     * Initialize exception with a reason for failure.
     * 
     * @param message reason for exception 
     * @param throwable root of the cause
     */
    public UnrecoverableException(final String message, final Throwable throwable) {
        super(message, throwable);
    }
}
