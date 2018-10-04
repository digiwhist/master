package eu.dl.server.exceptions;

/**
 * This exception(or its subclasses) should be thrown in case the user was not authenticated.
 *
 * @author Kuba Krafka
 */
public class NotAuthenticatedException extends RuntimeException {

    /**
     * Initialize exception with a reason for failure.
     *
     * @param message
     *            reason for exception
     */
    public NotAuthenticatedException(final String message) {
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
    public NotAuthenticatedException(final String message, final Throwable throwable) {
        super(message, throwable);
    }
}
