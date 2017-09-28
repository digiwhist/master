package eu.dl.core;

import org.hamcrest.CoreMatchers;
import org.junit.Assert;

/**
 * Extension to easily assert exception in jUnit tests.
 */
public final class ThrowableAssertion {

    /**
     * Use this assert to verify that your method has thrown proper exception.
     * 
     * @param exceptionThrower
     *            the source of the exception
     * @return instance of self usable to check other exception params
     */
    public static ThrowableAssertion assertThrown(final ExceptionThrower exceptionThrower) {
        try {
            exceptionThrower.throwException();
        } catch (Throwable caught) {
            return new ThrowableAssertion(caught);
        }
        throw new ExceptionNotThrownAssertionError();
    }

    private final Throwable caught;

    /**
     * Default constructor.
     * 
     * @param caught
     *            thrown exception
     */
    public ThrowableAssertion(final Throwable caught) {
        this.caught = caught;
    }

    /**
     * Assert checking whether the exception is instance of the proper exception
     * class.
     * 
     * @param exceptionClass
     *            expected exception class
     * @return itself for fluent interface
     */
    // CHECKSTYLE:OFF
    public ThrowableAssertion isInstanceOf(final Class<? extends Throwable> exceptionClass) {
        Assert.assertThat(caught, CoreMatchers.isA((Class<Throwable>) exceptionClass));
        return this;
    }
    // CHECKSTYLE:ON

    /**
     * Assert checking whether the exception has proper message.
     * 
     * @param expectedMessage
     *            expected message
     * @return itself for fluent interface
     */
    public ThrowableAssertion hasMessage(final String expectedMessage) {
        Assert.assertThat(caught.getMessage(), CoreMatchers.equalTo(expectedMessage));
        return this;
    }

    /**
     * Assert checking whether the exception has cause.
     * 
     * @return itself for fluent interface
     */
    public ThrowableAssertion hasNoCause() {
        Assert.assertThat(caught.getCause(), CoreMatchers.nullValue());
        return this;
    }

    /**
     * Assert checking whether the exception has cause of proper type.
     * 
     * @param exceptionClass
     *            expected cause exception class
     * @return itself for fluent interface
     */
    // CHECKSTYLE:OFF
    public ThrowableAssertion hasCauseInstanceOf(final Class<? extends Throwable> exceptionClass) {

        Assert.assertThat(caught.getCause(), CoreMatchers.notNullValue());
        Assert.assertThat(caught.getCause(), CoreMatchers.isA((Class<Throwable>) exceptionClass));
        return this;
    }
    // CHECKSTYLE:ON
}
