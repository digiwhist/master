package eu.datlab.worker.bg.parsed;

import java.util.ArrayList;
import java.util.List;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.select.Elements;

import eu.datlab.dataaccess.dto.parsed.ParsedContractingAuthority;
import eu.datlab.worker.parser.BaseContractingAuthorityParser;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * Contracting authority parsed for Bulgaria.
 *
 * @author Michal Riha
 */
public class AOPContractingAuthorityParser extends BaseContractingAuthorityParser {
    private static final String VERSION = "1";

    @Override
    public final List<ParsedContractingAuthority> parse(final RawData rawContractingAuthority) {
        final Document doc = Jsoup.parse(rawContractingAuthority.getSourceData());
        final Elements bulgariaDetail = doc.select("table[id=resultaTable] td");
        final List<ParsedContractingAuthority> list = new ArrayList();

        if (!bulgariaDetail.isEmpty()) {
            final ParsedContractingAuthority parsedContractingAuthority = new ParsedContractingAuthority();

            parsedContractingAuthority.setOfficialName(bulgariaDetail.get(1).text());

            parsedContractingAuthority.setVat(bulgariaDetail.get(3).text());
            final ParsedAddress parsedAddress = new ParsedAddress();
            parsedAddress.setStreet(bulgariaDetail.get(5).text());
            parsedAddress.setCity(bulgariaDetail.get(7).text());
            parsedAddress.setPostcode(bulgariaDetail.get(9).text());
            parsedAddress.setCountry(bulgariaDetail.get(11).text());
            parsedContractingAuthority.setAddress(parsedAddress);

            parsedContractingAuthority.setContactPoint(bulgariaDetail.get(13).text());
            parsedContractingAuthority.setPhone(bulgariaDetail.get(15).text());
            parsedContractingAuthority.setFax(bulgariaDetail.get(17).text());
            parsedContractingAuthority.setEmail(bulgariaDetail.get(19).text());
            parsedContractingAuthority.setUrl(bulgariaDetail.get(21).text());

            list.add(parsedContractingAuthority);
        }

        return list;
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final List<ParsedContractingAuthority> postProcessSourceSpecificRules(final List<ParsedContractingAuthority> parsed,
                                                                                    final RawData raw) {
        return parsed;
    }
}
