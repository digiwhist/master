package eu.datlab.worker.it.parsed;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.select.Elements;

import eu.datlab.dataaccess.dto.parsed.ParsedContractingAuthority;
import eu.datlab.worker.parser.BaseContractingAuthorityParser;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * Contracting authority parsed for Italy.
 *
 * @author Michal Riha
 */
public class IndicepaContractingAuthorityParser extends BaseContractingAuthorityParser {
    @Override
    public final List<ParsedContractingAuthority> parse(final RawData rawContractingAuthority) {
        Document document = Jsoup.parse(rawContractingAuthority.getSourceData());
        ParsedContractingAuthority parsedContractingAuthority = new ParsedContractingAuthority();

        Elements elements = document.select("div.scheda-ente-div > table > tbody > tr > td.desc");

        parsedContractingAuthority.setOfficialName(document.select("div.scheda-title > h2").first().text())
                .setAddress(new ParsedAddress().setRawAddress(elements.get(0).text()))
                .setUrl(elements.get(2).text())
                .setEmail(elements.get(3).text())
                .setType(elements.get(4).text())
                .setMainActivity(elements.get(5).text())
                .setContactName(elements.get(8).text())
                .setVat(elements.get(9).text());

        return new ArrayList<>(Arrays.asList(parsedContractingAuthority));
    }

    @Override
    public final String getVersion() {
        return null;
    }

    @Override
    protected final List<ParsedContractingAuthority> postProcessSourceSpecificRules(final List<ParsedContractingAuthority> parsed,
                                                                                    final RawData raw) {
        return parsed;
    }
}
