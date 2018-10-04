package eu.dl.server.exceptions;

/**
 * This exception(or its subclasses) should be thrown in case the user was not authorized for an API action.
 *
 * @author Kuba Krafka
 */
public class NotAuthorizedException extends RuntimeException {

    /**
     * Initialize exception with a reason for failure.
     *
     * @param message
     *            reason for exception
     */
    public NotAuthorizedException(final String message) {
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
    public NotAuthorizedException(final String message, final Throwable throwable) {
        super(message, throwable);
    }
}
