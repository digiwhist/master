package eu.dl.dataaccess.dao;

import eu.dl.dataaccess.dto.PlainDocument;

/**
 * Plain document DAO. Specifies methods for storing and loading plain documents.
 *
 * @author Tomas Mrazek
 */
public interface PlainDocumentDAO {
    /**
     * Saves given plain document to persistent storage.
     *
     * @param document
     *         plain document to be saved
     *
     * @return Id of saved plain document
     */
    String save(PlainDocument document);

    /**
     * Returns the plain document by given id.
     *
     * @param id
     *         id to be searched
     *
     * @return parsed item with given id
     */
    PlainDocument getById(String id);

    /**
     * Returns new instance of PlainDocument.
     *
     * @return empty instance
     */
    PlainDocument getEmptyInstance();

    /**
     * Returns the plain document by the given hash.
     *
     * @param hash
     *         hash to be set
     *
     * @return plain document with given hash
     */
    PlainDocument getLastByHash(String hash);

    /**
     * Removes object identified by the id.
     *
     * @param id
     *            unique identifier
     * @return true only and only if the record removing was successful
     */
    Boolean removeById(String id);
}
