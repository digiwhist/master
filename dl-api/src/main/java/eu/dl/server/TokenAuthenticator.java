package eu.dl.server;

import eu.dl.core.config.Config;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jws;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.MalformedJwtException;
import io.jsonwebtoken.SignatureException;
import org.pac4j.core.context.WebContext;
import org.pac4j.core.credentials.Credentials;
import org.pac4j.core.credentials.TokenCredentials;
import org.pac4j.core.credentials.authenticator.Authenticator;
import org.pac4j.core.exception.CredentialsException;
import org.pac4j.core.exception.HttpAction;
import org.pac4j.core.profile.CommonProfile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDateTime;

/**
 * Authenticates JWT token.
 */
public class TokenAuthenticator implements Authenticator {

    private final Config config = Config.getInstance();

    private final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

    @Override
    public final void validate(final Credentials credentials, final WebContext context) throws HttpAction, CredentialsException {
        try {
            String token = ((TokenCredentials) credentials).getToken();
            Jws<Claims> jwsToken = Jwts.parser().setSigningKey(config.getParam("authentication.apiSecurityKey")).parseClaimsJws(token);

            Claims claims = jwsToken.getBody();
            LocalDateTime expiration = LocalDateTime.parse((String) claims.get("expiration"));
            if (LocalDateTime.now().compareTo(expiration) > 0) {
                logger.error("Unable to authenticate, expired token used.");
                throw new CredentialsException("Expired token used.");
            }
            final CommonProfile profile = new CommonProfile();
            profile.setId(token);
            credentials.setUserProfile(profile);
        } catch (SignatureException e) {
            logger.error("Unable to authenticate, signature verification failed.");
            throw new CredentialsException("Signature verification failed.");
        } catch (MalformedJwtException e) {
            logger.error("Unable to authenticate, malformed token provided.");
            throw new CredentialsException("Malformed token provided.");
        }
    }
}
