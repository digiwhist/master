package eu.digiwhist.server;

import static spark.Spark.halt;

import org.pac4j.core.context.HttpConstants;
import org.pac4j.sparkjava.DefaultHttpActionAdapter;
import org.pac4j.sparkjava.SparkWebContext;

/**
 * Adapter used to generate error messages for the api.
 *
 * @author Kuba Krafka
 */
public class HttpActionAdapter extends DefaultHttpActionAdapter {

    @Override
    public final Object adapt(final int code, final SparkWebContext context) {
        if (code == HttpConstants.UNAUTHORIZED) {
            halt(401, "error401.mustache");
        } else if (code == HttpConstants.FORBIDDEN) {
            halt(403, "error403.mustache");
        } else {
            return super.adapt(code, context);
        }
        return null;
    }
}
