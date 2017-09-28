package eu.digiwhist.worker.nl.parsed;

import java.util.ArrayList;
import java.util.List;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import eu.digiwhist.dataaccess.dto.parsed.ParsedContractingAuthority;
import eu.digiwhist.worker.parser.BaseContractingAuthorityParser;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * This class is parsing contracting authorities data for Netherlands. Source
 * data of raw contracting authority contains several contracting authorities.
 *
 * @author Tomas Mrazek
 */
public class TenderNedContractingAuthorityParser extends BaseContractingAuthorityParser {
    private static final String VERSION = "1";

    @Override
    public final List<ParsedContractingAuthority> parse(final RawData rawContractingAuthority) {
        final Document doc = Jsoup.parse(rawContractingAuthority.getSourceData());
        final Elements nodes = doc.select("table[id=aanbestedendedienstlist-form:dt1] tbody tr");
        final List<ParsedContractingAuthority> list = new ArrayList();

        for (final Element node : nodes) {
            final ParsedContractingAuthority parsedContractingAuthority = new ParsedContractingAuthority();

            final ParsedAddress parsedAddress = new ParsedAddress();
            parsedAddress.setCountry(node.select("td:eq(2)").text());
            parsedAddress.setCity(node.select("td:eq(1)").text());

            parsedContractingAuthority.setAddress(parsedAddress);
            parsedContractingAuthority.setOfficialName(node.select("td:eq(0)").text());
            parsedContractingAuthority.setType(node.select("td:eq(3)").text());
            parsedContractingAuthority.setMainActivity(node.select("td:eq(4)").text());

            list.add(parsedContractingAuthority);
        }

        return list;
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }
}
