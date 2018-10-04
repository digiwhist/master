package eu.dl.server;

import org.pac4j.core.client.Clients;
import org.pac4j.core.config.Config;
import org.pac4j.core.config.ConfigFactory;
import org.pac4j.http.client.direct.ParameterClient;

/**
 * configuration factory used for APIs.
 */
public class ServerConfigFactory implements ConfigFactory {


    @Override
    public final Config build(final Object... parameters) {
        // REST authent with JWT for a token passed in the url as the token parameter
        ParameterClient parameterClient = new ParameterClient("auth_token", new TokenAuthenticator());
        parameterClient.setSupportGetRequest(true);
        parameterClient.setSupportPostRequest(true);

        final Clients clients = new Clients("http://localhost:8080/callback", parameterClient);

        final Config config = new Config(clients);
        config.setHttpActionAdapter(new HttpActionAdapter());
        return config;
    }
}