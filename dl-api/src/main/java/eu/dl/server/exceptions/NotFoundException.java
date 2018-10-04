package eu.dl.server.exceptions;

/**
 * This exception(or its subclasses) should be thrown in case the item was not found.
 * There are proper exception handlers registered on the BaseServer class.
 *
 * @author Kuba Krafka
 */
public class NotFoundException extends RuntimeException {

    /**
     * Initialize exception with a reason for failure.
     *
     * @param message
     *            reason for exception
     */
    public NotFoundException(final String message) {
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
    public NotFoundException(final String message, final Throwable throwable) {
        super(message, throwable);
    }
}
