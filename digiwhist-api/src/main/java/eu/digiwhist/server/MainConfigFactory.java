package eu.digiwhist.server;

import org.pac4j.core.client.Clients;
import org.pac4j.core.config.Config;
import org.pac4j.http.client.direct.DirectBasicAuthClient;
import org.pac4j.http.client.indirect.IndirectBasicAuthClient;
import org.pac4j.http.credentials.authenticator.test.SimpleTestUsernamePasswordAuthenticator;

/**
 * API configuration. Adds basic http authentication to the API when used.
 *
 * @author Kuba Krafka
 */
public class MainConfigFactory  implements org.pac4j.core.config.ConfigFactory {
    @Override
    public final Config build() {
        final IndirectBasicAuthClient client = new IndirectBasicAuthClient(
                        new SimpleTestUsernamePasswordAuthenticator());
        final DirectBasicAuthClient directBasicAuthClient = new DirectBasicAuthClient(
                        new SimpleTestUsernamePasswordAuthenticator());
        final Clients clients = new Clients("http://localhost:8080/callback", directBasicAuthClient, client);
        final Config config = new Config(clients);
        config.setHttpActionAdapter(new HttpActionAdapter());
        return config;
    }
}
