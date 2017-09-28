package eu.digiwhist.worker.fr;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Common utilities for France.
 *
 * @author Marek Mikes
 */
public final class BOAMPTenderUtils {
    /**
     * Private constructor to make this class static.
     */
    private BOAMPTenderUtils() {}

    /**
     * Pattern of publication permalink on the web.
     */
    public static final String PUBLICATION_PERMALINK_PATTERN = PublicationSources.FR_BOAMP_WEB + "/avis/detail/%s";

    /**
     * Metadata key for XML file path on disc.
     */
    public static final String FILE_PATH_METADATA_KEY = "filePath";
    /**
     * Metadata key for URL of archive where some file is.
     */
    public static final String ARCHIVE_URL_METADATA_KEY = "archiveUrl";
    /**
     * Metadata key for URL of HTML page of publication.
     */
    public static final String HTML_SOURCE_URL_METADATA_KEY = "htmlSourceUrl";
    /**
     * Metadata key for source data of HTML page publication.
     */
    public static final String HTML_SOURCE_DATA_METADATA_KEY = "htmlSourceData";

    /**
     * Form type mapping.
     */
    public static final Map<Enum, List<String>> FORM_TYPE_MAPPING = new HashMap<>();

    static {
        FORM_TYPE_MAPPING.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList(
                "Avis d'appel public à la concurrence Autres avis d'appel public",
                "Avis de mise en concurrence",
                "Rectificatif - Avis de marché",
                "Délégation de service public",
                "Concessions",
                "Avis en cas de transparence ex ante volontaire",
                "Avis d'intention de conclure",
                "Avis d'appel public à la concurrence Avis pour 12 mois",
                "Avis d'appel public à la concurrence Délai d'urgence",
                "Procédures accélérées",
                "appel_offre"));
        FORM_TYPE_MAPPING.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList(
                "Avis d'attribution",
                "Résultat de marché",
                "Avis d'attribution comportant des lots infructueux",
                "Annulation - Rectificatif - Sans suite - Infructueux",
                "Résultat de marché Infructueux - Sans suite",
                "Résultat de marché Résultat de marché comportant des lots infructueux",
                "Rectificatif - Avis d'attribution",
                "Annulation - Rectificatif Avis en cas de transparence ex ante volontaire",
                "attribution"));
        FORM_TYPE_MAPPING.put(PublicationFormType.CONTRACT_UPDATE, Arrays.asList(
                "Rectificatif"));
        FORM_TYPE_MAPPING.put(PublicationFormType.CONTRACT_CANCELLATION, Arrays.asList(
                "Annulation - Rectificatif",
                "Infructueux - Sans suite",
                "Annulation",
                "Annulation - Avis de marché",
                "Annulation - Avis d'attribution"));
        FORM_TYPE_MAPPING.put(PublicationFormType.PRIOR_INFORMATION_NOTICE, Arrays.asList(
                "Avis informatifs",
                "Rectificatif - Avis en cas de transparence ex ante volontaire",
                "pre-information"));
    }

    /**
     * The method checks whether document has new format or not.
     *
     * @param fileName
     *         name of XML file
     *
     * @return true if document has new format; otherwise false
     */
    public static boolean isNewXmlFormat(final String fileName) {
        return fileName.contains("-");
    }

    /**
     * The method parses publication source ID from new XML filename.
     *
     * @param newXmlFilename
     *         name of new XML file
     *
     * @return publication source ID
     */
    public static String getPublicationSourceIdFrom(final String newXmlFilename) {
        assert newXmlFilename.endsWith(".xml");
        return newXmlFilename.lastIndexOf('/') != -1
                ? newXmlFilename.substring(newXmlFilename.lastIndexOf('/') + 1, newXmlFilename.length() - 4)
                : newXmlFilename.substring(0, newXmlFilename.length() - 4);
    }

}
