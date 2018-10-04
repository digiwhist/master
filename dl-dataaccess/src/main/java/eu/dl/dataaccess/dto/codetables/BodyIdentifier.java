package eu.dl.dataaccess.dto.codetables;

import com.fasterxml.jackson.annotation.JsonIgnore;
import eu.dl.dataaccess.dto.clean.Validable;
import eu.dl.dataaccess.utils.ValidationUtils;

/**
 * Body identifier.
 */
public class BodyIdentifier implements Validable {
    /**
     * Identifier.
     */
    private String id;

    /**
     * Type of identifier.
     */
    private Type type;

    /**
     * Scope under which the identifier is unique (typically country code, EU
     * for VAT or null for global identifier).
     */
    private Scope scope;

    /**
     * @return the id
     */
    public final String getId() {
        return id;
    }

    /**
     * @param id
     *         the id to set
     *
     * @return this instance for chaining
     */
    public final BodyIdentifier setId(final String id) {
        this.id = id;
        return this;
    }

    /**
     * @return the type
     */
    public final Type getType() {
        return type;
    }

    /**
     * @param type
     *         the type to set
     *
     * @return this instance for chaining
     */
    public final BodyIdentifier setType(final Type type) {
        this.type = type;
        return this;
    }

    /**
     * @return the scope
     */
    public final Scope getScope() {
        return scope;
    }

    /**
     * @param scope
     *         the scope to set
     *
     * @return this instance for chaining
     */
    public final BodyIdentifier setScope(final Scope scope) {
        this.scope = scope;
        return this;
    }

    @Override
    @JsonIgnore
    public final BodyIdentifier getValid() {
        return ValidationUtils.getValid(this, id);
    }

    /**
     * Type of body identifier.
     */
    public enum Type {
        /**
         * Etalon ID - a special identifier used in a MatchedBody as reference on the EtalonBody from which the
         * MatchedBody was created.
         */
        ETALON_ID,
        
        /**
         * Bvd ID - Bureau van Dajk ID.
         */
        BVD_ID,
        
        /**
         * Value-added tax ID.
         */
        VAT,

        /**
         * Organization ID (eg. ICO in Czech Republic).
         */
        ORGANIZATION_ID,

        /**
         * Organization ID (ICO) from Vestnik form header.
         */
        HEADER_ICO,
                
        /**
         * Tax ID.
         */
        TAX_ID,
        
        /**
         * Treda register ID.
         */
        TRADE_REGISTER,
        
        /**
         * Statistical ID.
         */
        STATISTICAL,

        /**
         * Source ID (eg. internal id used in source application).
         */
        SOURCE_ID;
    }

    /**
     * Scope, under which the identifier is unique.
     */
    public enum Scope {
        /**
         * Unknown scope.
         */
        UNKNOWN,

        /**
         * Etalon ID - a special identifier used in a MatchedBody as reference on the EtalonBody from which the
         * MatchedBody was created.
         */
        ETALON_ID,
        
        /**
         * Unique within the European Union.
         */
        EU,

        /**
         * Globally unique.
         */
        GLOBAL,

        
        /**
         * The following list of codes is a complete ISO 3166-1 encoding list of the countries which are assigned
         * official codes (https://en.wikipedia.org/wiki/ISO_3166-1).
         */
        
        /**
         *  Andorra.
         */
        AD,
        /** 
         * United Arab Emirates.
         */
        AE,
        /** 
         * Afghanistan.
         */
        AF,
        /** 
         * Antigua and Barbuda.
         */
        AG,
        /** 
         * Anguilla.
         */
        AI,
        /** 
         * Albania.
         */
        AL,
        /** 
         * Armenia.
         */
        AM,
        /** 
         * Angola.
         */
        AO,
        /** 
         * Antarctica.
         */
        AQ,
        /** 
         * Argentina.
         */
        AR,
        /** 
         * American Samoa.
         */
        AS,
        /** 
         * Austria.
         */
        AT,
        /** 
         * Australia.
         */
        AU,
        /** 
         * Aruba.
         */
        AW,
        /** 
         * Åland Islands.
         */
        AX,
        /** 
         * Azerbaijan.
         */
        AZ,
        /** 
         * Bosnia and Herzegovina.
         */
        BA,
        /** 
         * Barbados.
         */
        BB,
        /** 
         * Bangladesh.
         */
        BD,
        /** 
         * Belgium.
         */
        BE,
        /** 
         * Burkina Faso.
         */
        BF,
        /** 
         * Bulgaria.
         */
        BG,
        /** 
         * Bahrain.
         */
        BH,
        /** 
         * Burundi.
         */
        BI,
        /** 
         * Benin.
         */
        BJ,
        /** 
         * Saint Barthélemy.
         */
        BL,
        /** 
         * Bermuda.
         */
        BM,
        /** 
         * Brunei Darussalam.
         */
        BN,
        /** 
         * Bolivia (Plurinational State of).
         */
        BO,
        /** 
         * Bonaire, Sint Eustatius and Saba.
         */
        BQ,
        /** 
         * Brazil.
         */
        BR,
        /** 
         * Bahamas.
         */
        BS,
        /** 
         * Bhutan.
         */
        BT,
        /** 
         * Bouvet Island.
         */
        BV,
        /** 
         * Botswana.
         */
        BW,
        /** 
         * Belarus.
         */
        BY,
        /** 
         * Belize.
         */
        BZ,
        /** 
         * Canada.
         */
        CA,
        /** 
         * Cocos (Keeling) Islands.
         */
        CC,
        /** 
         * Congo (Democratic Republic of the).
         */
        CD,
        /** 
         * Central African Republic.
         */
        CF,
        /** 
         * Congo.
         */
        CG,
        /** 
         * Switzerland.
         */
        CH,
        /** 
         * Côte d'Ivoire.
         */
        CI,
        /** 
         * Cook Islands.
         */
        CK,
        /** 
         * Chile.
         */
        CL,
        /** 
         * Cameroon.
         */
        CM,
        /** 
         * China.
         */
        CN,
        /** 
         * Colombia.
         */
        CO,
        /** 
         * Costa Rica.
         */
        CR,
        /** 
         * Cuba.
         */
        CU,
        /**
         * Cabo Verde.
         */
        CV,
        /** 
         * Curaçao.
         */
        CW,
        /** 
         * Christmas Island.
         */
        CX,
        /** 
         * Cyprus.
         */
        CY,
        /** 
         * Czechia.
         */
        CZ,
        /** 
         * Germany.
         */
        DE,
        /** 
         * Djibouti.
         */
        DJ,
        /** 
         * Denmark.
         */
        DK,
        /** 
         * Dominica.
         */
        DM,
        /** 
         * Dominican Republic.
         */
        DO,
        /** 
         * Algeria.
         */
        DZ,
        /** 
         * Ecuador.
         */
        EC,
        /** 
         * Estonia.
         */
        EE,
        /** 
         * Egypt.
         */
        EG,
        /** 
         * Western Sahara.
         */
        EH,
        /** 
         * Eritrea.
         */
        ER,
        /** 
         * Spain.
         */
        ES,
        /** 
         * Ethiopia.
         */
        ET,
        /** 
         * Finland.
         */
        FI,
        /** 
         * Fiji.
         */
        FJ,
        /** 
         * Falkland Islands (Malvinas).
         */
        FK,
        /** 
         * Micronesia (Federated States of).
         */
        FM,
        /** 
         * Faroe Islands.
         */
        FO,
        /** 
         * France.
         */
        FR,
        /** 
         * Gabon.
         */
        GA,
        /** 
         * United Kingdom of Great Britain and Northern Ireland.
         */
        GB,
        /** 
         * Grenada.
         */
        GD,
        /** 
         * Georgia.
         */
        GE,
        /** 
         * French Guiana.
         */
        GF,
        /** 
         * Guernsey.
         */
        GG,
        /** 
         * Ghana.
         */
        GH,
        /** 
         * Gibraltar.
         */
        GI,
        /** 
         * Greenland.
         */
        GL,
        /** 
         * Gambia.
         */
        GM,
        /** 
         * Guinea.
         */
        GN,
        /** 
         * Guadeloupe.
         */
        GP,
        /** 
         * Equatorial Guinea.
         */
        GQ,
        /** 
         * Greece.
         */
        GR,
        /** 
         * South Georgia and the South Sandwich Islands.
         */
        GS,
        /** 
         * Guatemala.
         */
        GT,
        /** 
         * Guam.
         */
        GU,
        /** 
         * Guinea-Bissau.
         */
        GW,
        /** 
         * Guyana.
         */
        GY,
        /** 
         * Hong Kong.
         */
        HK,
        /** 
         * Heard Island and McDonald Islands.
         */
        HM,
        /** 
         * Honduras.
         */
        HN,
        /** 
         * Croatia.
         */
        HR,
        /** 
         * Haiti.
         */
        HT,
        /** 
         * Hungary.
         */
        HU,
        /** 
         * Indonesia.
         */
        ID,
        /** 
         * Ireland.
         */
        IE,
        /** 
         * Israel.
         */
        IL,
        /** 
         * Isle of Man.
         */
        IM,
        /** 
         * India.
         */
        IN,
        /** 
         * British Indian Ocean Territory.
         */
        IO,
        /** 
         * Iraq.
         */
        IQ,
        /** 
         * Iran (Islamic Republic of).
         */
        IR,
        /** 
         * Iceland.
         */
        IS,
        /** 
         * Italy.
         */
        IT,
        /** 
         * Jersey.
         */
        JE,
        /** 
         * Jamaica.
         */
        JM,
        /** 
         * Jordan.
         */
        JO,
        /** 
         * Japan.
         */
        JP,
        /** 
         * Kenya.
         */
        KE,
        /** 
         * Kyrgyzstan.
         */
        KG,
        /** 
         * Cambodia.
         */
        KH,
        /** 
         * Kiribati.
         */
        KI,
        /** 
         * Comoros.
         */
        KM,
        /** 
         * Saint Kitts and Nevis.
         */
        KN,
        /** 
         * Korea (Democratic People's Republic of).
         */
        KP,
        /** 
         * Korea (Republic of).
         */
        KR,
        /** 
         * Kuwait.
         */
        KW,
        /** 
         * Cayman Islands.
         */
        KY,
        /** 
         * Kazakhstan.
         */
        KZ,
        /** 
         * Lao People's Democratic Republic.
         */
        LA,
        /** 
         * Lebanon.
         */
        LB,
        /** 
         * Saint Lucia.
         */
        LC,
        /** 
         * Liechtenstein.
         */
        LI,
        /** 
         * Sri Lanka.
         */
        LK,
        /** 
         * Liberia.
         */
        LR,
        /** 
         * Lesotho.
         */
        LS,
        /** 
         * Lithuania.
         */
        LT,
        /** 
         * Luxembourg.
         */
        LU,
        /** 
         * Latvia.
         */
        LV,
        /** 
         * Libya.
         */
        LY,
        /** 
         * Morocco.
         */
        MA,
        /** 
         * Monaco.
         */
        MC,
        /** 
         * Moldova (Republic of).
         */
        MD,
        /** 
         * Montenegro.
         */
        ME,
        /** 
         * Saint Martin (French part).
         */
        MF,
        /** 
         * Madagascar.
         */
        MG,
        /** 
         * Marshall Islands.
         */
        MH,
        /** 
         * Macedonia (the former Yugoslav Republic of).
         */
        MK,
        /** 
         * Mali.
         */
        ML,
        /** 
         * Myanmar.
         */
        MM,
        /** 
         * Mongolia.
         */
        MN,
        /** 
         * Macao.
         */
        MO,
        /** 
         * Northern Mariana Islands.
         */
        MP,
        /** 
         * Martinique.
         */
        MQ,
        /** 
         * Mauritania.
         */
        MR,
        /** 
         * Montserrat.
         */
        MS,
        /** 
         * Malta.
         */
        MT,
        /** 
         * Mauritius.
         */
        MU,
        /** 
         * Maldives.
         */
        MV,
        /** 
         * Malawi.
         */
        MW,
        /** 
         * Mexico.
         */
        MX,
        /** 
         * Malaysia.
         */
        MY,
        /** 
         * Mozambique.
         */
        MZ,
        /** 
         * Namibia.
         */
        NA,
        /** 
         * New Caledonia.
         */
        NC,
        /** 
         * Niger.
         */
        NE,
        /** 
         * Norfolk Island.
         */
        NF,
        /** 
         * Nigeria.
         */
        NG,
        /** 
         * Nicaragua.
         */
        NI,
        /** 
         * Netherlands.
         */
        NL,
        /** 
         * Norway.
         */
        NO,
        /** 
         * Nepal.
         */
        NP,
        /** 
         * Nauru.
         */
        NR,
        /** 
         * Niue.
         */
        NU,
        /** 
         * New Zealand.
         */
        NZ,
        /** 
         * Oman.
         */
        OM,
        /** 
         * Panama.
         */
        PA,
        /** 
         * Peru.
         */
        PE,
        /** 
         * French Polynesia.
         */
        PF,
        /** 
         * Papua New Guinea.
         */
        PG,
        /** 
         * Philippines.
         */
        PH,
        /** 
         * Pakistan.
         */
        PK,
        /** 
         * Poland.
         */
        PL,
        /** 
         * Saint Pierre and Miquelon.
         */
        PM,
        /** 
         * Pitcairn.
         */
        PN,
        /** 
         * Puerto Rico.
         */
        PR,
        /** 
         * Palestine, State of.
         */
        PS,
        /** 
         * Portugal.
         */
        PT,
        /** 
         * Palau.
         */
        PW,
        /** 
         * Paraguay.
         */
        PY,
        /** 
         * Qatar.
         */
        QA,
        /** 
         * Réunion.
         */
        RE,
        /** 
         * Romania.
         */
        RO,
        /** 
         * Serbia.
         */
        RS,
        /** 
         * Russian Federation.
         */
        RU,
        /** 
         * Rwanda.
         */
        RW,
        /** 
         * Saudi Arabia.
         */
        SA,
        /** 
         * Solomon Islands.
         */
        SB,
        /** 
         * Seychelles.
         */
        SC,
        /** 
         * Sudan.
         */
        SD,
        /** 
         * Sweden.
         */
        SE,
        /** 
         * Singapore.
         */
        SG,
        /** 
         * Saint Helena, Ascension and Tristan da Cunha.
         */
        SH,
        /** 
         * Slovenia.
         */
        SI,
        /** 
         * Svalbard and Jan Mayen.
         */
        SJ,
        /** 
         * Slovakia.
         */
        SK,
        /** 
         * Sierra Leone.
         */
        SL,
        /** 
         * San Marino.
         */
        SM,
        /** 
         * Senegal.
         */
        SN,
        /** 
         * Somalia.
         */
        SO,
        /** 
         * Suriname.
         */
        SR,
        /** 
         * South Sudan.
         */
        SS,
        /** 
         * Sao Tome and Principe.
         */
        ST,
        /** 
         * El Salvador.
         */
        SV,
        /** 
         * Sint Maarten (Dutch part).
         */
        SX,
        /** 
         * Syrian Arab Republic.
         */
        SY,
        /** 
         * Swaziland.
         */
        SZ,
        /** 
         * Turks and Caicos Islands.
         */
        TC,
        /** 
         * Chad.
         */
        TD,
        /** 
         * French Southern Territories.
         */
        TF,
        /** 
         * Togo.
         */
        TG,
        /** 
         * Thailand.
         */
        TH,
        /** 
         * Tajikistan.
         */
        TJ,
        /** 
         * Tokelau.
         */
        TK,
        /** 
         * Timor-Leste.
         */
        TL,
        /** 
         * Turkmenistan.
         */
        TM,
        /** 
         * Tunisia.
         */
        TN,
        /** 
         * Tonga.
         */
        TO,
        /** 
         * Turkey.
         */
        TR,
        /** 
         * Trinidad and Tobago.
         */
        TT,
        /** 
         * Tuvalu.
         */
        TV,
        /** 
         * Taiwan, Province of China[a].
         */
        TW,
        /** 
         * Tanzania, United Republic of.
         */
        TZ,
        /** 
         * Ukraine.
         */
        UA,
        /** 
         * Uganda.
         */
        UG,
        /** 
         * United States Minor Outlying Islands.
         */
        UM,
        /** 
         * United States of America.
         */
        US,
        /** 
         * Uruguay.
         */
        UY,
        /** 
         * Uzbekistan.
         */
        UZ,
        /** 
         * Holy See.
         */
        VA,
        /** 
         * Saint Vincent and the Grenadines.
         */
        VC,
        /** 
         * Venezuela (Bolivarian Republic of).
         */
        VE,
        /** 
         * Virgin Islands (British).
         */
        VG,
        /** 
         * Virgin Islands (U.S.).
         */
        VI,
        /** 
         * Viet Nam.
         */
        VN,
        /** 
         * Vanuatu.
         */
        VU,
        /** 
         * Wallis and Futuna.
         */
        WF,
        /** 
         * Samoa.
         */
        WS,
        /** 
         * Yemen.
         */
        YE,
        /** 
         * Mayotte.
         */
        YT,
        /** 
         * South Africa.
         */
        ZA,
        /** 
         * Zambia.
         */
        ZM;
    }
}
