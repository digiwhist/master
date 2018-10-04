package eu.datlab.worker.hu.parsed;

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
 * This class is parsing contracting authority data for Hungary.
 *
 * @author Tomas Mrazek
 */
public class KHContractingAuthorityParser extends BaseContractingAuthorityParser {
    private static final String VERSION = "1";

    @Override
    public final List<ParsedContractingAuthority> parse(final RawData rawContractingAuthority) {
        final Document doc = Jsoup.parse(rawContractingAuthority.getSourceData());

        final ParsedContractingAuthority parsedContractingAuthority = new ParsedContractingAuthority();

        final Elements tbl = doc.select("table.ertesitofej tbody");

        parsedContractingAuthority.setOfficialName(tbl.select("tr:eq(0) td").get(0).text());
        parsedContractingAuthority.setNationalRegistrationNumber(tbl.select("tr:eq(1) td").get(0).text());

        final ParsedAddress parsedAddress = new ParsedAddress();
        parsedAddress.setRawAddress(tbl.select("tr:eq(2) td").get(0).text());
        parsedContractingAuthority.setAddress(parsedAddress);

        final List<ParsedContractingAuthority> list = new ArrayList();
        list.add(parsedContractingAuthority);
        return list;
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }
}
