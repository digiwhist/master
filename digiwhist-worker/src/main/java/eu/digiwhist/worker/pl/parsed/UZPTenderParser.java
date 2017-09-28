package eu.digiwhist.worker.pl.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.digiwhist.worker.parser.BaseDigiwhistTenderParser;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.parser.Parser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Poland xml parsed.
 *
 * @author Tomas Mrazek
 */
public class UZPTenderParser extends BaseDigiwhistTenderParser {
    private static final String VERSION = "1.1";

    private static final Logger logger = LoggerFactory.getLogger(UZPTenderParser.class);

    /**
     * Parses the given raw tender object.
     *
     * @param rawTender
     *         raw tender to be parsed
     *
     * @return list of parsed tenders or empty list if none tenders have been parsed
     */
    @Override
    public final List<ParsedTender> parse(final RawData rawTender) {
        final Document document = Jsoup.parse(rawTender.getSourceData(), "", Parser.xmlParser());

        String machineReadableUrl = rawTender.getSourceUrl() + "?" + rawTender.getSourceFileName();

        ParsedTender parsedTender = parseCommonFormData(document, machineReadableUrl);

        parsedTender = parseFormSpecificData(parsedTender, document);

        return new ArrayList<>(Arrays.asList(parsedTender));
    }

    /**
     * Returns actual version of this parsed manager.
     *
     * @return actual parsed manager version
     */
    @Override
    public final String getVersion() {
        return VERSION;
    }

    /**
     * Parses common data and returns initialized parsed tender.
     *
     * @param document
     *         parsed xml document
     * @param machineReadableUrl
     *          machine readable url for included publication
     * @return parsed tender
     */
    private ParsedTender parseCommonFormData(final Document document, final String machineReadableUrl) {
        final ParsedTender parsedTender = new ParsedTender();

        parsedTender
            .addPublication(new ParsedPublication()
                .setIsIncluded(true)
                .setSource(PublicationSources.PL_UZP_FTP)
                .setSourceId(JsoupUtils.selectText("pozycja", document))
                .setPublicationDate(JsoupUtils.selectText("data_publikacji", document))
                .setSourceFormType(JsoupUtils.selectText("ogloszenie", document))
                .setMachineReadableUrl(machineReadableUrl))
            .addBuyer(parseBuyer(document))
            .setHasLots(parseHasLots(document))
            .setTitle(JsoupUtils.selectText("nazwa_zamowienia, nazwa_konkursu", document))
            .setDescription(JsoupUtils.selectText("przedmiot_zam", document))
            .setSupplyType(JsoupUtils.selectText("rodz_zam", document))
            .setProcedureType(JsoupUtils.selectText("kod_trybu", document))
            .setCpvs(UZPTenderParserUtils.parseCPVs(document));

        return parsedTender;
    }

    /**
     * Parse if tender has lots.
     *
     * @param document document to parse from
     * @return String or null
     */
    private String parseHasLots(final Document document) {
        final String hasLots = JsoupUtils.selectText("czy_czesci", document);

        if (hasLots == null) {
            return null;
        } else if (hasLots.contains("Y")) {
            return Boolean.TRUE.toString();
        } else if (hasLots.contains("N")) {
            return Boolean.FALSE.toString();
        } else {
            return null;
        }
    }

    /**
     * Parses buyer from the given document.
     * 
     * @param document
     *          parsed document
     * @return parsed buyer
     */
    private ParsedBody parseBuyer(final Document document) {
        ParsedBody buyer = new ParsedBody()
            .setName(JsoupUtils.selectText("nazwa", document))
            .setAddress(new ParsedAddress()
                .setStreet(JsoupUtils.selectText("ulica", document, true) + " "
                    + JsoupUtils.selectText("nr_domu", document, true))
                .setCity(JsoupUtils.selectText("miejscowosc", document))
                .setPostcode(JsoupUtils.selectText("kod_poczt", document))
                .setState(JsoupUtils.selectText("wojewodztwo", document))
                .setUrl(JsoupUtils.selectText("internet", document)))
            .setPhone(JsoupUtils.selectText("tel", document))            
            .setEmail(JsoupUtils.selectText("e_mail", document))
            .setBuyerType(JsoupUtils.selectText("rodzaj_zam_inny, rodzaj_zam", document));

        String bodyId = JsoupUtils.selectText("regon", document);
        if (bodyId != null) {
            buyer.addBodyId(new BodyIdentifier()
                .setId(bodyId)
                .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                .setScope(BodyIdentifier.Scope.EU));
        }

        return buyer;
    }

    /**
     * Parses form specific data.
     *
     * @param parsedTender
     *         parsed tender
     * @param document
     *         parsed xml document
     *
     * @return parsed Tender
     * @throws UnrecoverableException
     *         in case that form type is unsupported
     */
    private ParsedTender parseFormSpecificData(final ParsedTender parsedTender, final Document document) {
        final String formType = getFormType(document);
        switch (formType) {
            //Contract award notice
            case "ZP-403":
            case "ZP-405":
            case "ZP-408":
                return UZPContractAwardNoticeHandler.parse(parsedTender, document);
            //Contract notice
            case "ZP-400":
            case "ZP-401":
            case "ZP-402":
            case "ZP-404":
                return UZPContractNoticeHandler.parse(parsedTender, document);
            //Modification
            case "ZP-SPR":
            case "ZP-406":
                return UZPModificationHandler.parse(parsedTender, document);
            default:
                return UZPDefaultHandler.parse(parsedTender, document);
        }
    }

    /**
     * Gets type of form for parsed document.
     *
     * @param context
     *         context
     *
     * @return type of form
     */
    private static String getFormType(final Element context) {
        return JsoupUtils.getRoot(context).tagName().toUpperCase();
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "PL";
    }
}
