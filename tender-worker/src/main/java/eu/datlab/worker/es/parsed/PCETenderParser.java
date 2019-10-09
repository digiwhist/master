package eu.datlab.worker.es.parsed;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;

/**
 * This class parses tender data for Spain.
 *
 * @author Tomas Mrazek
 */
public class PCETenderParser extends BaseDatlabTenderParser {

    private static final String VERSION = "1";

    private static final Logger logger = LoggerFactory.getLogger(PCETenderParser.class);
    /**
     * HTML detail - form type.
     */
    private static final int RAW_HTML_DETAIL = 1;
    /**
     * XML document - Contract Notice.
     */
    private static final int XML_CONTRACT_NOTICE = 2;
    /**
     * XML document - Contract Award.
     */
    private static final int XML_CONTRACT_AWARD = 3;
    /**
     * XML document - Call for tender.
     */
    private static final int XML_CALL_FOR_TENDER = 4;
    /**
     * XML document - Modification.
     */
    private static final int XML_CONTRACT_UPDATE = 5;

    /**
     * Parses the given raw tender object.
     *
     * @param rawTender
     *            raw tender to be parsed
     *
     * @return list of parsed tenders or empty list if none tenders have been
     *         parsed
     * @throws eu.dl.core.UnrecoverableException
     *             if no parsed could be created for given raw tender
     */
    @Override
    public final List<ParsedTender> parse(final RawData rawTender) {
        final Document document = Jsoup.parse(rawTender.getSourceData());

        ParsedTender parsedTender = new ParsedTender();
        String tenderUrl = rawTender.getSourceUrl().toString();

        if (rawTender.getSourceUrl().toString().startsWith("https://contrataciondelestado.es/wps/poc")) {
            parsedTender = PCETenderHtmlDetailHandler.parse(parsedTender, document, "HtmlDetail");
        } else {
            final Element formTypeElement = JsoupUtils.select("body > *:eq(0)", document).first();

            if (formTypeElement == null) {
                logger.debug("Unknown format of publication on ()", rawTender.getSourceUrl());
                throw new UnrecoverableException("Unknown format of publication on " + rawTender.getSourceUrl());
            }

            final String formType = formTypeElement.tagName();

            if (formType.equalsIgnoreCase("ContractAwardNotice")) {
                parsedTender = PCETenderXmlContractCommonHandler.parse(parsedTender, document, tenderUrl, formType);
                parsedTender = PCETenderXmlContractAwardHandler.parse(parsedTender, document);
            } else if (formType.equalsIgnoreCase("CallForTenders")) {
                parsedTender = PCETenderXmlContractCommonHandler.parse(parsedTender, document, tenderUrl, formType);
                parsedTender = PCETenderXmlCallForTenderHandler.parse(parsedTender, document);
            } else {
                parsedTender = PCETenderXmlContractCommonHandler.parse(parsedTender, document, tenderUrl, formType);
            }
        }

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

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "ES";
    }
}
