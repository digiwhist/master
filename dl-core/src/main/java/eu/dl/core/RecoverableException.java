package eu.dl.core;

/**
 * This exception(or its subclasses) should be thrown in case the worker failed,
 * but the original message should be resend to another worker. For example, the
 * resource is temporary unavailable. Use it with care, as throwing this can
 * easily cause infinite loops, or create big load on remote resources in case
 * of crawlers.
 *
 * @author Kuba Krafka
 */
public class RecoverableException extends RuntimeException {

    private static final long serialVersionUID = 2490165236268497218L;

    /**
     * Initialize exception with a reason for failure.
     *
     * @param message
     *            reason for exception
     */
    public RecoverableException(final String message) {
        super(message);
    }

    /**
     * Initialize exception with a reason for failure.
     *
     * @param message
     *            reason for exception
     * @param throwable
     *            root of the cause
     */
    public RecoverableException(final String message, final Throwable throwable) {
        super(message, throwable);
    }
}
