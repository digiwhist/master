package eu.dl.dataaccess.dto.ocds;

import eu.dl.dataaccess.annotation.Transformable;
import java.util.ArrayList;
import java.util.List;

/**
 * Base class for OCDS entities that refers to list of documents.
 *
 * @param <T>
 *      class of underlying object
 * @author Tomas Mrazek
 */
@Transformable
public abstract class BaseOCDSDocumentsReferrer<T> {
    private List<OCDSDocument> documents;
    
    /**
     * @return list of related documents
     */
    public final List<OCDSDocument> getDocuments() {
        return documents;
    }

    /**
     * @param newDocuments
     *      list of related documents to be set
     * @return this instance for chaining
     */
    public final T setDocuments(final List<OCDSDocument> newDocuments) {
        this.documents = newDocuments;
        return (T) this;
    }

    /**
     * Adds document. List is created if needed.
     *
     * @param document
     *      related document to be added
     * @return this instance for chaining
     */
    public final T addDocument(final OCDSDocument document) {
        if (document != null) {
            if (this.documents == null) {
                this.documents = new ArrayList<>();
            }

            this.documents.add(document);
        }

        return (T) this;
    }

    /**
     * Adds documents. List is created if needed.
     *
     * @param newDocuments
     *      list of related documents to be added
     * @return this instance for chaining
     */
    public final T addDocuments(final List<OCDSDocument> newDocuments) {
        if (newDocuments != null && !newDocuments.isEmpty()) {
            if (this.documents == null) {
                this.documents = new ArrayList<>();
            }

            this.documents.addAll(newDocuments);
        }

        return (T) this;
    }
}
