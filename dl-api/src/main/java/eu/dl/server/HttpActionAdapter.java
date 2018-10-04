package eu.dl.server;

import eu.dl.server.exceptions.NotAuthenticatedException;
import eu.dl.server.exceptions.NotAuthorizedException;
import org.pac4j.core.context.HttpConstants;
import org.pac4j.sparkjava.DefaultHttpActionAdapter;
import org.pac4j.sparkjava.SparkWebContext;

/**
 * Default Adapter responsible for handling not authorized and not authenticated events.
 */
public class HttpActionAdapter extends DefaultHttpActionAdapter {

    @Override
    public final Object adapt(final int code, final SparkWebContext context) {
        if (code == HttpConstants.UNAUTHORIZED) {
            throw new NotAuthenticatedException("User is not authenticated.");
        } else if (code == HttpConstants.FORBIDDEN) {
            throw new NotAuthorizedException("User is not authorized for this action.");
        } else {
            return super.adapt(code, context);
        }
    }
}
