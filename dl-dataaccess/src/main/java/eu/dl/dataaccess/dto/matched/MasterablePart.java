package eu.dl.dataaccess.dto.matched;

import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * Provides ability to get tender ID of the object.
 */
public interface MasterablePart {
    /**
     * @return tender ID
     */
    String getTenderId();

    /**
     * @return publication date
     */
    LocalDate getPublicationDate();

    /**
     * @return created date
     */
    LocalDateTime getCreatedRaw();

    /**
     * @param publicationDate publication date
     * @return this for fluent interface
     */
    MasterablePart setPublicationDate(LocalDate publicationDate);
}
