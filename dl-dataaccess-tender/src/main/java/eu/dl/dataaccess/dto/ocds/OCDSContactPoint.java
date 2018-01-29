package eu.dl.dataaccess.dto.ocds;


import com.fasterxml.jackson.annotation.JsonProperty;
import eu.dl.dataaccess.annotation.Transformable;
import java.net.URL;

/**
 * OCDS contact point. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 */
@Transformable
public class OCDSContactPoint {

    private String name;

    private String email;

    private String phone;
    
    private URL url;

    /**
     * @return name
     */
    public final String getName() {
        return name;
    }

    /**
     * @param name
     *      name to be set
     * @return this instance for chaining
     */
    public final OCDSContactPoint setName(final String name) {
        this.name = name;
        return this;
    }

    /**
     * @return email
     */
    public final String getEmail() {
        return email;
    }

    /**
     * @param email
     *      email t be set
     * @return this instance for chaining
     */
    public final OCDSContactPoint setEmail(final String email) {
        this.email = email;
        return this;
    }

    /**
     * @return phone
     */
    @JsonProperty("telephone")
    public final String getPhone() {
        return phone;
    }

    /**
     * @param phone
     *      phone to be set
     * @return this instance chaining
     */
    public final OCDSContactPoint setPhone(final String phone) {
        this.phone = phone;
        return this;
    }

    /**
     * @return url
     */
    public final URL getUrl() {
        return url;
    }

    /**
     * @param url
     *      url to be set
     * @return this instance for chaining
     */
    public final OCDSContactPoint setUrl(final URL url) {
        this.url = url;
        return this;
    }
}
