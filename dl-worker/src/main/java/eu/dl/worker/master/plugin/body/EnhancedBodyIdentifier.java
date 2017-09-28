package eu.dl.worker.master.plugin.body;

import java.time.LocalDate;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;

/**
 * This class is used in bodyId matching to store regular body identifier data
 * together with extended information about the body such a publicationDate,
 * source etc.
 *
 */
public class EnhancedBodyIdentifier extends BodyIdentifier {
    private Boolean etalon;

    private LocalDate publicationDate;

    /**
     * Constructs enhanced body identifier based on regular one.
     * 
     * @param bodyIdentifier
     *            "base" for the identifier
     * @param etalon
     *            whether its comming from etalon body
     * @param publicationDate
     *            body publication date
     */
    public EnhancedBodyIdentifier(final BodyIdentifier bodyIdentifier, final Boolean etalon,
            final LocalDate publicationDate) {
        this.etalon = etalon;
        this.publicationDate = publicationDate;
        this.setId(bodyIdentifier.getId());
        this.setScope(bodyIdentifier.getScope());
        this.setType(bodyIdentifier.getType());
    }

    /**
     * @return the etalon
     */
    public final Boolean getEtalon() {
        return etalon;
    }

    /**
     * @param etalon
     *            the etalon to set
     */
    public final void setEtalon(final Boolean etalon) {
        this.etalon = etalon;
    }

    /**
     * @return the publicationDate
     */
    public final LocalDate getPublicationDate() {
        return publicationDate;
    }

    /**
     * @param publicationDate
     *            the publicationDate to set
     */
    public final void setPublicationDate(final LocalDate publicationDate) {
        this.publicationDate = publicationDate;
    }
}
