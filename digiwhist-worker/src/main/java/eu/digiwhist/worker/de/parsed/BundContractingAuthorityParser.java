package eu.digiwhist.worker.de.parsed;

import java.util.ArrayList;
import java.util.List;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.select.Elements;

import eu.digiwhist.dataaccess.dto.parsed.ParsedContractingAuthority;
import eu.digiwhist.worker.parser.BaseContractingAuthorityParser;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * This class is parsing contracting authority data for Germany.
 *
 * @author Tomas Mrazek
 */
public class BundContractingAuthorityParser extends BaseContractingAuthorityParser {
    private static final String VERSION = "1";

    @Override
    public final List<ParsedContractingAuthority> parse(final RawData rawContractingAuthority) {
        final Document doc = Jsoup.parse(rawContractingAuthority.getSourceData());

        final ParsedContractingAuthority parsedContractingAuthority = new ParsedContractingAuthority();

        final Elements contact = doc.select(".content-box .text section:eq(0)");
        final ParsedAddress parsedAddress = new ParsedAddress();
        parsedAddress.setRawAddress(contact.select("address").text());

        parsedContractingAuthority.setOfficialName(doc.select(".content-box .orgUnit h1").get(0).text());
        parsedContractingAuthority.setAddress(parsedAddress);
        parsedContractingAuthority.setPhone(contact.select(".col > .contact dt:contains(telefon:) + dd").text());
        parsedContractingAuthority.setFax(contact.select(".col > .contact dt:contains(fax:) + dd").text());
        parsedContractingAuthority.setEmail(contact.select(".col > .contact dt:contains(e-mail:) + dd").text());
        parsedContractingAuthority.setUrl(contact.select(".orgUnitHomepage .value").text());

        List<ParsedContractingAuthority> list = new ArrayList();
        list.add(parsedContractingAuthority);
        return list;
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }
}
