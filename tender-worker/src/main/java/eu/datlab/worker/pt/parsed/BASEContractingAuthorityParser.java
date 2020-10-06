package eu.datlab.worker.pt.parsed;

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
 * Contract authority parsed for Portugal.
 *
 * @author Michal Riha
 */
public final class BASEContractingAuthorityParser extends BaseContractingAuthorityParser {
    private static final String VERSION = "1";

    @Override
    public List<ParsedContractingAuthority> parse(final RawData rawContractingAuthority) {

        // source contains several contracting authorities
        final Document doc = Jsoup.parse(rawContractingAuthority.getSourceData());
        final Elements companyPageTable = doc.select("table[id=resultadosAnuncios] td");
        final List<ParsedContractingAuthority> list = new ArrayList();

        if (!companyPageTable.isEmpty()) {
            final ParsedContractingAuthority parsedContractingAuthority = new ParsedContractingAuthority();

            parsedContractingAuthority.setOfficialName(companyPageTable.get(3).text());
            final ParsedAddress parsedAddress = new ParsedAddress();
            parsedAddress.setRawAddress(companyPageTable.get(5).text());
            parsedContractingAuthority.setAddress(parsedAddress);
            parsedContractingAuthority.setContractsAsCA(companyPageTable.get(7).text());
            parsedContractingAuthority.setExpenditures(companyPageTable.get(9).text());
            parsedContractingAuthority.setTendersWon(companyPageTable.get(13).text());
            parsedContractingAuthority.setEarnings(companyPageTable.get(15).text());

            list.add(parsedContractingAuthority);
        }

        return list;
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected List<ParsedContractingAuthority> postProcessSourceSpecificRules(final List<ParsedContractingAuthority> parsed,
                                                                                    final RawData raw) {
        return parsed;
    }
}
