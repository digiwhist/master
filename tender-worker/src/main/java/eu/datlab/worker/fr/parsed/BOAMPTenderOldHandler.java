package eu.datlab.worker.fr.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.fr.BOAMPTenderUtils;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.clean.utils.CodeTableUtils;
import eu.dl.worker.utils.StringUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.parser.Parser;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Old form parser for France.
 *
 * @author Marek Mikes
 */
final class BOAMPTenderOldHandler {
    private static final Logger logger = LoggerFactory.getLogger(BOAMPTenderOldHandler.class);

    /**
     * Private constructor to make this class static.
     */
    private BOAMPTenderOldHandler() {}

    /**
     * Parses data for all old forms (contract notice and contract award).
     *
     * @param rawTender
     *            raw tender to be parsed
     *
     * @return List<ParsedTender> with parsed data
     */
    static List<ParsedTender> parse(final RawData rawTender) {
        List<ParsedTender> parsedTenders = new ArrayList<>();

        final Document document = Jsoup.parse(rawTender.getSourceData(), "", Parser.xmlParser());
        final String publicationDate = JsoupUtils.selectAttribute("PARUTION_BOAMP", "dateparution", document);
        Elements publicationElements = JsoupUtils.select("ANNONCE_REF", document);
        for (Element publicationElement : publicationElements) {
            // set common attributes
            ParsedTender parsedTender = new ParsedTender()
                .setPublications(parsePublications(publicationElement, rawTender.getSourceUrl().toString(),
                    publicationDate))
                .addBuyer(new ParsedBody()
                    .setName(JsoupUtils.selectText("DONNEES > IDENT > NOM", publicationElement))
                    .setContactName(JsoupUtils.selectText("DONNEES > IDENT > PRM", publicationElement))
                    .setAddress(new ParsedAddress()
                        .setStreet(JsoupUtils.selectText("DONNEES > IDENT > ADRESSE", publicationElement))
                        .setPostcode(JsoupUtils.selectText("DONNEES > IDENT > CP", publicationElement))
                        .setCity(JsoupUtils.selectText("DONNEES > IDENT > VILLE", publicationElement)))
                    .setPhone(JsoupUtils.selectText("DONNEES > IDENT > TEL", publicationElement))
                    .setEmail(StringUtils.removeDotsAtTheEnd(
                        JsoupUtils.selectText("DONNEES > IDENT > MEL", publicationElement)))
                    .setMainActivities(parseBuyerMainActivities(publicationElement)))
                .setTitle(JsoupUtils.selectText("DONNEES > OBJET > OBJET_COMPLET, GESTION > K1", publicationElement))
                .setAddressOfImplementation(new ParsedAddress()
                    .setRawAddress(StringUtils.removeDotsAtTheEnd(
                        JsoupUtils.selectText("DONNEES > OBJET > LIEU_LIVR, DONNEES > OBJET > LIEU_EXEC_LIVR, DONNEES > OBJET > LIEU_EXEC",
                            publicationElement)))
                    .addNuts(StringUtils.removeDotsAtTheEnd(
                        JsoupUtils.selectText("DONNEES > OBJET > CODE_NUTS", publicationElement))))
                .setCpvs(parseTenderCpvs(publicationElement))
                .setSupplyType(parseTenderSupplyType(publicationElement))
                .setAppealBodyName(JsoupUtils.selectText(
                    "DONNEES > PROCEDURES_RECOURS > INSTANCE_RECOURS > ACHETEUR", publicationElement))
                .setAwardDecisionDate(JsoupUtils.selectText("DONNEES > PROCEDURES > DATE_ATT", document))
                .setProcedureType(JsoupUtils.selectAttribute("DONNEES > PROCEDURE", "type", publicationElement))
                .setDescription(JsoupUtils.selectCombinedText("DESCRIPTEURS > DESCRIPTEUR", publicationElement))
                .setEstimatedPrice(new ParsedPrice()
                    .setNetAmount(JsoupUtils.selectText("DONNEES > PROCEDURES > VALEUR_ESTIMEE + AUTRES", document)));

            final PublicationFormType formType = (PublicationFormType) CodeTableUtils.mapValue(
                    parsedTender.getPublications().get(0).getSourceFormType(), BOAMPTenderUtils.FORM_TYPE_MAPPING,
                    PublicationFormType.OTHER, false);
            // some publications do not have the form type filled. E.g. 15-27168
            if (formType != null) {
                switch (formType) {
                    case CONTRACT_NOTICE:
                        BOAMPTenderOldContractNoticeHandler.parse(publicationElement, parsedTender);
                        break;
                    case CONTRACT_AWARD:
                        BOAMPTenderOldContractAwardHandler.parse(publicationElement, parsedTender);
                        break;
                    default:
                        break;
                }
            }

            parsedTenders.add(parsedTender);
        }

        return parsedTenders;
    }

    /**
     * Parses info about this publication and all the related ones.
     *
     * @param publicationElement
     *      publication element to be parsed
     * @param machineReadableUrl
     *         machine readable URL to be set
     * @param publicationDate
     *         publication date
     *
     * @return list of publications
     */
    private static List<ParsedPublication> parsePublications(final Element publicationElement,
                                                             final String machineReadableUrl,
                                                             final String publicationDate) {
        List<ParsedPublication> publications = new ArrayList<>();

        final String sourceId = JsoupUtils.selectText("GESTION > NOJO", publicationElement);
        // publication in XML file
        publications.add(new ParsedPublication()
                .setIsIncluded(true)
                .setSourceFormType(JsoupUtils.selectText("GESTION > TETIER_R4", publicationElement))
                .setPublicationDate(publicationDate)
                .setMachineReadableUrl(machineReadableUrl)
                .setHumanReadableUrl(String.format(BOAMPTenderUtils.PUBLICATION_PERMALINK_PATTERN, sourceId))
                .setSource(PublicationSources.FR_BOAMP_FTP)
                .setSourceId(sourceId));

        return publications;
    }

    /**
     * Parses list of buyer main activities.
     *
     * @param publicationElement
     *      publication element to be parsed
     * @return list of buyer main activities
     */
    private static List<String> parseBuyerMainActivities(final Element publicationElement) {
        Element mainActivityParentElement = JsoupUtils.selectFirst("DONNEES > TYPE_ACTIVITE_ORG", publicationElement);
        if (mainActivityParentElement == null) {
            return null;
        }
        Elements mainActivityElements = mainActivityParentElement.children();
        return mainActivityElements
                .stream()
                .map(e -> e.nodeName())
                .collect(Collectors.toList());
    }

    /**
     * Parse tender CPVs from publication element.
     *
     * @param publicationElement
     *         publication element to be parsed
     *
     * @return list of parsed CPVs or Null
     */
    private static List<ParsedCPV> parseTenderCpvs(final Element publicationElement) {
        List<ParsedCPV> tenderCpvs = new ArrayList<>();

        String mainCpvCode = JsoupUtils.selectText("DONNEES > OBJET > CPV_OBJ", publicationElement);
        if (mainCpvCode != null) {
            tenderCpvs.add(new ParsedCPV()
                    .setCode(StringUtils.removeDotsAtTheEnd(mainCpvCode))
                    .setIsMain(Boolean.TRUE.toString()));
        }

        String complementCpvCodesString = JsoupUtils.selectText("DONNEES > OBJET > CPV_COMPLEMENT", publicationElement);
        if (complementCpvCodesString != null) {
            // E.g. "<CPV_COMPLEMENT>48781000, 48782000, 50000000.</CPV_COMPLEMENT>"
            complementCpvCodesString = StringUtils.removeDotsAtTheEnd(complementCpvCodesString);
            String[] complementCpvCodes = complementCpvCodesString.split(",");
            for (String complementCpvCode : complementCpvCodes) {
                tenderCpvs.add(new ParsedCPV()
                        .setCode(StringUtils.removeDotsAtTheEnd(complementCpvCode))
                        .setIsMain(Boolean.FALSE.toString()));
            }
        }

        return tenderCpvs.isEmpty() ? null : tenderCpvs;
    }

    /**
     * Parse tender supply type value from publication element.
     *
     * @param publicationElement
     *         publication element to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderSupplyType(final Element publicationElement) {
        // annotation says that the supply type is "DONNEES > OBJET > AUTRES", but it is not probably true. We parse the
        // supply type from another element:
        final Elements supplyTypeElements = JsoupUtils.select("DONNEES > CLASSES > CLASSE", publicationElement);
        if (supplyTypeElements.isEmpty()) {
            return null;
        } else {
            if (supplyTypeElements.size() > 1) {
                logger.warn("We can parse more than one supply type, but we parse the first one");
            }
            return supplyTypeElements.get(0).text();
        }
    }

}
