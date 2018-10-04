package eu.dl.server.exceptions;

/**
 * This exception(or its subclasses) should be thrown in case there is an error in parameters handling.
 *
 * @author Kuba Krafka
 */
public class ParameterFormattingException extends RuntimeException {

    /**
     * Initialize exception with a reason for failure.
     *
     * @param message
     *            reason for exception
     */
    public ParameterFormattingException(final String message) {
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
    public ParameterFormattingException(final String message, final Throwable throwable) {
        super(message, throwable);
    }
}
