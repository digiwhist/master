package eu.datlab.worker.no.parsed;

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
 * Contract authority parsed for Norway.
 *
 * @author Michal Riha
 */
public class DOFFINContractingAuthorityParser extends BaseContractingAuthorityParser {
    private static final String VERSION = "1";

    @Override
    public final List<ParsedContractingAuthority> parse(final RawData rawContractingAuthority) {

        final Document doc = Jsoup.parse(rawContractingAuthority.getSourceData());
        final Elements companyPageTable = doc.select("div[class=row-fluid] > div[class=span4]");
        final List<ParsedContractingAuthority> list = new ArrayList();

        if (!companyPageTable.isEmpty()) {
            final ParsedContractingAuthority parsedContractingAuthority = new ParsedContractingAuthority();

            final Elements nameAndDescriptions = companyPageTable.get(0).select("div > div >div");
            parsedContractingAuthority.setOfficialName(nameAndDescriptions.get(0).text());
            parsedContractingAuthority.setVat(nameAndDescriptions.get(2).text());

            final Elements contactInformations = companyPageTable.get(1).select("div > div > div");
            final ParsedAddress parsedAddress = new ParsedAddress();
            parsedAddress.setRawAddress(contactInformations.get(0).text());
            parsedContractingAuthority.setAddress(parsedAddress);
            parsedContractingAuthority.setContactName(contactInformations.get(1).text());
            parsedContractingAuthority.setEmail(contactInformations.get(2).text());
            if (contactInformations.size() > 3) {
                parsedContractingAuthority.setPhone(contactInformations.get(3).text());
            }

            list.add(parsedContractingAuthority);
        }

        return list;
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }
}
