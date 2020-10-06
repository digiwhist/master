package eu.datlab.worker.ee.parsed;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * Tender parser for E-procurement in Estonia.
 * 
 * @author Tomas Mrazek
 */
public final class EPETenderParser extends BaseDatlabTenderParser {

    private static final String VERSION = "1.0";

    @Override
    public List<ParsedTender> parse(final RawData raw) {
        // It is necessary to replace &nbsp; with space. Jsoup use unicode character \u00a0 for '&nbsp;' and this little
        // detail causes many issues when creating xpaths and selectors because space has two variants, the ordinary
        // space and the unicode character.
        Document doc = Jsoup.parse(raw.getSourceData().replace("&nbsp;", " "));
        
        HashMap<String, Object> metaData = raw.getMetaData();
        String contractingAuthority = (String) metaData.get("contractingAuthority");
        String sourceFormType = (String) metaData.get("sourceFormType");
        String publicationDate = (String) metaData.get("publicationDate");

        ParsedTender parsedTender;
        switch (sourceFormType) {
            // contract notice
            case "TV02": case "TV05":
                parsedTender = EPEContractNoticeHandler.parse(doc, publicationDate);
                break;
            // contract award
            case "TV15":
                parsedTender = EPEContractAwardHandler.parse(doc, publicationDate);
                break;
            // design contest result
            case "TV13":
                parsedTender = EPEDesignContestResultHandler.parse(doc, publicationDate);
                break;
            // anex
            case "TV15L":
                parsedTender = EPEContractAnexHandler.parse(doc, publicationDate);
                break;
            //Invitation to a design contest
            case "TV12":
                parsedTender = EPEInvitationToADesignContestHandler.parse(doc, publicationDate);
                break;
            default:
                logger.warn("Unknown form type {}, default handler was used", sourceFormType);
                parsedTender = EPEDefaultHandler.parse(doc, publicationDate);
        }
        
        return parsedTender != null ? Collections.singletonList(parsedTender) : Collections.emptyList();
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "EE";
    }

    @Override
    protected List<ParsedTender> postProcessSourceSpecificRules(final List<ParsedTender> parsed, final RawData raw) {
        return parsed;
    }
}
