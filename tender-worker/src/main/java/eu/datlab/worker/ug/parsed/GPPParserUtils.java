package eu.datlab.worker.ug.parsed;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.ocds.OCDSAward;
import eu.dl.dataaccess.dto.ocds.OCDSContract;
import eu.dl.dataaccess.dto.ocds.OCDSOrganization;
import eu.dl.dataaccess.dto.ocds.OCDSOrganizationReference;
import eu.dl.dataaccess.dto.ocds.OCDSPeriod;
import eu.dl.dataaccess.dto.ocds.OCDSRelease;
import eu.dl.dataaccess.dto.ocds.OCDSTender;
import eu.dl.dataaccess.dto.ocds.OCDSValue;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;

/**
 * Utils class for Uganda parsers.
 */
public final class GPPParserUtils {

    /**
     * Suppress default constructor.
     */
    private GPPParserUtils() {
    }

    /**
     * For the given reference attempts to find match in list of organizations. If such organization exists returns organization as
     * parsed body instance.
     *
     * @param ref
     *      OCDS organization reference
     * @param bodies
     *      list of OCDS organizations
     * @return parsed body or null
     */
    public static ParsedBody getBody(final OCDSOrganizationReference ref, final List<OCDSOrganization> bodies) {
        if (ref == null || ref.getId() == null || bodies == null) {
            return null;
        }

        OCDSOrganization body = bodies.stream().filter(n -> Objects.equals(n.getId(), ref.getId())).findFirst().orElse(null);
        if (body == null) {
            body = bodies.stream().filter(n -> Objects.equals(n.getName(), ref.getName())).findFirst().orElse(null);
        }

        return parseBody(body);
    }

    /**
     * @param body
     *      OCDS organization
     * @return parsed body
     */
    public static ParsedBody parseBody(final OCDSOrganization body) {
        if (body == null) {
            return null;
        }

        ParsedBody parsedBody = new ParsedBody()
            .addBodyId(new BodyIdentifier().setType(BodyIdentifier.Type.SOURCE_ID).setScope(BodyIdentifier.Scope.UG).setId(body.getId()))
            .setName(body.getName());

        if (body.getContactPoint() != null) {
            parsedBody
                .setContactName(body.getContactPoint().getName())
                .setEmail(body.getContactPoint().getEmail())
                .setPhone(body.getContactPoint().getPhone());
        }

        if (body.getAddress() != null) {
            parsedBody.setAddress(new ParsedAddress()
                .setCountry(body.getAddress().getCountry())
                .setCity(body.getAddress().getLocality())
                .setRawAddress(body.getAddress().getPostcode()));
        }

        return parsedBody;
    }

    /**
     * @param value
     *      OCDS value
     * @return parsed price
     */
    public static ParsedPrice parsePrice(final OCDSValue value) {
        if (value == null) {
            return null;
        }

        return new ParsedPrice()
            .setNetAmount(numberToString(value.getAmount()))
            .setCurrency(value.getCurrency() != null ? value.getCurrency().toString() : null);
    }

    /**
     * @param input
     *      enumeration item
     * @return item name or null
     */
    public static String enumToString(final Enum input) {
        return Optional.ofNullable(input).map(Enum::name).orElse(null);
    }

    /**
     * @param input
     *      boolean
     * @return boolean as string or null
     */
    public static String booleanToString(final Boolean input) {
        return Optional.ofNullable(input).map(String::valueOf).orElse(null);
    }

    /**
     * @param input
     *      datetime
     * @return detatime as string or null
     */
    public static String datetimeToString(final LocalDateTime input) {
        return Optional.ofNullable(input).map(LocalDateTime::toString).orElse(null);
    }

    /**
     * @param period
     *      period
     * @param getter
     *      period getter function that returns datetime
     * @return datetime as string or null
     */
    public static String periodDatetimeToString(final OCDSPeriod period, final Function<OCDSPeriod, LocalDateTime> getter) {
        return Optional.ofNullable(period).map(n -> GPPParserUtils.datetimeToString(getter.apply(n))).orElse(null);
    }

    /**
     * @param input
     *      number
     * @return number as string or null
     */
    public static String numberToString(final Number input) {
        return Optional.ofNullable(input).map(Number::toString).orElse(null);
    }

    /**
     * @param r
     *      OCDS release
     * @param a
     *      OCDS award
     * @return OCDS contract for the given award or null
     */
    public static OCDSContract getContractForAward(final OCDSRelease r, final OCDSAward a) {
        if (r == null || a == null) {
            return null;
        }

        return r.getContracts().stream()
            .filter(c -> Objects.equals(a.getId(), c.getAwardId()))
            .findFirst().orElse(null);
    }

    /**
     * Parses national procedure type as procurement method enumeration value + ":" + description.
     * @param tender tender to parse
     * @return parsed national procedure type
     */
    public static String parseNationalProcedureType(final OCDSTender tender) {
        String enumValue = GPPParserUtils.enumToString(tender.getProcurementMethod());
        String description = tender.getProcurementMethodDetails();
        if((enumValue == null || enumValue.isEmpty()) && (description == null || description.isEmpty())) {
            return null;
        }
        if(enumValue == null) {
            enumValue = "";
        }
        if(description == null) {
            description = "";
        }
        return enumValue + ":" + description;
    }
}
