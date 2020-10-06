package eu.datlab.worker.es.parsed;

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
 * This class is parsing contracting authority data for Spain.
 *
 * @author Tomas Mrazek
 */
public class PCEContractingAuthorityParser extends BaseContractingAuthorityParser {
    private static final String VERSION = "1";

    @Override
    public final List<ParsedContractingAuthority> parse(final RawData rawContractingAuthority) {
        final Document doc = Jsoup.parse(rawContractingAuthority.getSourceData());

        final ParsedContractingAuthority parsedContractingAuthority = new ParsedContractingAuthority();

        final Elements main = doc.select("#DatosGenerales .divDatosGenerales");
        if (main.size() > 0) {
            parsedContractingAuthority.setOfficialName(
                    main.select("ul li:eq(0):contains(Órgano de Contratación:) + li").text());
            parsedContractingAuthority.setNationalRegistrationNumber(
                    main.select("ul li:eq(0):contains(NIF:) + li").text());
            parsedContractingAuthority.setUrl(main.select("ul li:eq(0):contains(Direct link:) + li a").attr("href"));
        }

        parsedContractingAuthority.setMainActivity(
                doc.select("#Actividades .divDatosGenerales ul:eq(0) li:eq(1)").text());

        final Elements address = doc.select("#Direccion .divDatosGenerales");
        if (address.size() > 0) {
            final ParsedAddress parsedAddress = new ParsedAddress();

            parsedAddress.setRawAddress(address.text());
            parsedAddress.setStreet(address.select("ul li:eq(0):contains(Street:) + li").text());
            parsedAddress.setPostcode(address.select("ul li:eq(0):contains(Postcode:) + li").text());
            parsedAddress.setCity(address.select("ul li:eq(0):contains(Town:) + li").text());
            parsedAddress.setCountry(address.select("ul li:eq(0):contains(Country:) + li").text());

            parsedContractingAuthority.setAddress(parsedAddress);
        }

        final Elements contact = doc.select("#Contacto .divDatosGenerales");
        if (contact.size() > 0) {
            parsedContractingAuthority.setPhone(contact.select("ul li:eq(0):contains(Phone no.:) + li").text());
            parsedContractingAuthority.setFax(contact.select("ul li:eq(0):contains(Fax:) + li").text());
            parsedContractingAuthority.setEmail(contact.select("ul li:eq(0):contains(Email:) + li").text());
        }

        final List<ParsedContractingAuthority> list = new ArrayList();
        list.add(parsedContractingAuthority);
        return list;
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }
}
