package eu.dl.dataaccess.dto.ocds;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import eu.dl.dataaccess.annotation.Transformable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * OCDS release package.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 */
@Transformable
public class OCDSReleasePackage {

    private String uri;

    private String version;

    private LocalDateTime published;

    private OCDSPublisher publisher;

    private List<String> extensions;

    private List<OCDSRelease> releases;

    /**
     * @return list of extensions
     */
    public final List<String> getExtensions() {
        return extensions;
    }

    /**
     * @param extensions
     *      list of extensions to be set
     * @return this instance for chaining
     */
    public final OCDSReleasePackage setExtensions(final List<String> extensions) {
        this.extensions = extensions;
        return this;
    }

    /**
     * Adds extension. List is created if needed.
     *
     * @param extension
     *      extension to be added
     * @return this instance for chaining
     */
    public final OCDSReleasePackage addExtension(final String extension) {
        if (extension != null) {
            if (this.extensions == null) {
                this.extensions = new ArrayList<>();
            }

            this.extensions.add(extension);
        }

        return this;
    }

    /**
     * @return list of releases
     */
    public final List<OCDSRelease> getReleases() {
        return releases;
    }

    /**
     * @param releases
     *      list of releases to be set
     * @return this instance for chaining
     */
    public final OCDSReleasePackage setReleases(final List<OCDSRelease> releases) {
        this.releases = releases;
        return this;
    }

    /**
     * Adds release. List is created if needed.
     *
     * @param release
     *      extension to be added
     * @return this instance for chaining
     */
    public final OCDSReleasePackage addRelease(final OCDSRelease release) {
        if (release != null) {
            if (this.releases == null) {
                this.releases = new ArrayList<>();
            }

            this.releases.add(release);
        }

        return this;
    }

    /**
     * @return uri
     */
    public final String getUri() {
        return uri;
    }

    /**
     * @param uri
     *      uri to be set
     * @return this instance for chaining
     */
    public final OCDSReleasePackage setUri(final String uri) {
        this.uri = uri;
        return this;
    }

    /**
     * @return version
     */
    public final String getVersion() {
        return version;
    }

    /**
     * @param version
     *      version to be set
     * @return this instance for chaining
     */
    public final OCDSReleasePackage setVersion(final String version) {
        this.version = version;
        return this;
    }

    /**
     * @return date of publishing
     */
    @JsonProperty("publishedDate")
    @JsonSerialize(using = OCDSLocalDateTimeSerializer.class)
    public final LocalDateTime getPublished() {
        return published;
    }

    /**
     * @param published
     *      date to be set
     * @return this instance for chaining
     */
    public final OCDSReleasePackage setPublished(final LocalDateTime published) {
        this.published = published;
        return this;
    }

    /**
     * @return publisher
     */
    public final OCDSPublisher getPublisher() {
        return publisher;
    }

    /**
     * @param publisher
     *      publisher to be set
     * @return this instance for chaining
     */
    public final OCDSReleasePackage setPublisher(final OCDSPublisher publisher) {
        this.publisher = publisher;
        return this;
    }
}
