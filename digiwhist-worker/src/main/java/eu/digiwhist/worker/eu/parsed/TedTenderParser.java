package eu.digiwhist.worker.eu.parsed;

import eu.digiwhist.worker.parser.BaseDigiwhistTenderParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * TED parsed.
 *
 * @author Michal Riha
 */
public class TedTenderParser extends BaseDigiwhistTenderParser {
    private static final String VERSION = "5.2";

    private static final Logger logger = LoggerFactory.getLogger(TedTenderParser.class);

    /**
     * Parses the given raw tender object.
     *
     * @param rawTender
     *         raw tender to be parsed
     *
     * @return list of parsed tenders or empty list if none tenders have been parsed
     * @throws UnrecoverableException
     *         if no parsed could be created for given raw tender
     */
    @Override
    public final List<ParsedTender> parse(final RawData rawTender) {
        final Document document = Jsoup.parse(rawTender.getSourceData());

        final TedFormType formType = TedTenderParserUtils.getFormType(document);

        final TedFormVersionType formVersion = TedTenderParserUtils.getFormVersion(document);

        //common data are same for each document type
        ParsedTender parsedTender = parseCommonFormData(document, formVersion);

        //form type specific
        parsedTender = parseFormSpecificData(parsedTender, document, formType, formVersion);

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
     *         parsed document
     * @param version
     *         form version
     *
     * @return parsed tender
     */
    private ParsedTender parseCommonFormData(final Document document, final TedFormVersionType version) {
        final ParsedTender parsedTender = new ParsedTender()
            .setSelectionMethod(TedTenderParserUtils.parseSelectionMethod(
                TedTenderParserUtils.getCodedDataNode(document)))
            .setTitleEnglish(JsoupUtils.selectText("ML_TI_DOC[LG=EN] > TI_TEXT",
                TedTenderParserUtils.getTranslationNode(document)))
            .setCountry(JsoupUtils.selectAttribute("TED_EXPORT > CODED_DATA_SECTION > NOTICE_DATA > ISO_COUNTRY",
                    "VALUE", document));

        //version specific common data
        if (version.is(TedFormVersionType.R209, TedFormVersionType.IS_OLDER)) {
            parsedTender
                .setTitle(JsoupUtils.selectText("TITLE_CONTRACT", TedTenderParserUtils.getOriginNode(document)));
        } else {
            parsedTender
                //main publication
                .addPublication(TedTenderParserR209Utils.parseMainPublication(document))
                //previous publication concerning this procedure
                .addPublication(TedTenderParserR209Utils.parsePreviousPublication(document));
            
            TedTenderParserUtils.appendNoticeReference(document, parsedTender);
        }

        return parsedTender;
    }

    /**
     * Parses form specific data.
     *
     * @param parsedTender
     *         parsed tender
     * @param document
     *         parsed document
     * @param formType
     *         TED form type
     * @param version
     *         form version
     *
     * @return parsed tender
     * @throws UnrecoverableException
     *         in case that form type is unsupported
     */
    private ParsedTender parseFormSpecificData(final ParsedTender parsedTender, final Document document,
            final TedFormType formType, final TedFormVersionType version) {
        final boolean isPriorR209 = version.is(TedFormVersionType.R209, TedFormVersionType.IS_OLDER);

        switch (formType) {
            case CONTRACT_NOTICE:
            case PRIOR_INFORMATION_NOTICE:
                if (isPriorR209) {
                    if (formType == TedFormType.CONTRACT_NOTICE) {
                        return TedContractNoticeHandler.parse(parsedTender, document);
                    } else {
                        return TedPriorInformationNoticeHandler.parse(parsedTender, document);
                    }
                } else {
                    return TedContractNoticeHandlerR209.parse(parsedTender, document);
                }
            case CONTRACT_NOTICE_UTILITIES:
                if (isPriorR209) {
                    return TedContractNoticeUtilitiesHandler.parse(parsedTender, document);
                } else {
                    return TedContractNoticeUtilitiesHandlerR209.parse(parsedTender, document);
                }
            case CONTRACT_AWARD_NOTICE:
                if (isPriorR209) {
                    return TedContractAwardHandler.parse(parsedTender, document);
                } else {
                    return TedContractAwardHandlerR209.parse(parsedTender, document);
                }
            case CONTRACT_AWARD_NOTICE_UTILITIES:
                if (isPriorR209) {
                    return TedContractAwardUtilitiesHandler.parse(parsedTender, document);
                } else {
                    return TedContractAwardUtilitiesHandlerR209.parse(parsedTender, document);
                }
            case CORRECTION:
                if (isPriorR209) {
                    return TedCorrigendumHandler.parse(parsedTender, document);
                } else {
                    return TedCorrigendumHandlerR209.parse(parsedTender, document);
                }
            default:
                logger.error("No parsed implemented for {} form type.", formType.getCode());
                throw new UnrecoverableException("Could not create appropriate parsed.");
        }
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return null;
    }
}
