/*
 * The MIT License
 *
 * Copyright 2017 Datlab.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package eu.datlab.worker.ee.clean;

import eu.datlab.worker.ee.parsed.EPEParserUtils;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedBody;

import org.junit.Test;


import static org.junit.Assert.assertEquals;


/**
 * Tests of Estonia parser.
 *
 * @author Tomas Mrazek
 */
public class EPEParserUtilsTest {

    /**
     * Tests of successful body parsing with {@link EPEParserUtils#parseBody(java.lang.String)}.
     */
    @Test
    public final void parseBodyTest() {
        // at least name and country is needed
        ParsedBody body = EPEParserUtils.parseBody("Maiken jaanisk Eesti (EE)");
        assertEquals("Maiken jaanisk", body.getName());
        assertEquals("EE", body.getAddress().getCountry());



        body = EPEParserUtils.parseBody("Lastwagen StutZ GmbH, 482089, Zugerstrasse 47, Zufikon, CH-5621"
            + " Šveits (CH)");

        assertEquals("Lastwagen StutZ GmbH", body.getName());
        assertEquals("Zufikon", body.getAddress().getCity());
        assertEquals("Zugerstrasse 47", body.getAddress().getStreet());
        assertEquals("CH", body.getAddress().getCountry());
        assertEquals("CH-5621", body.getAddress().getPostcode());


        
        body = EPEParserUtils.parseBody("Viljandi Ühendatud Kutsekeskkool, 70005565, Vana-Võidu, 70101 Eesti (EE)"
            + " Kontaktisik: Mati Valli Tel.: 4351037 Faks: 4351022 E-post: mati@vykk.vil.ee");

        assertEquals("Viljandi Ühendatud Kutsekeskkool", body.getName());
        assertEquals("Mati Valli", body.getContactName());
        assertEquals("mati@vykk.vil.ee", body.getEmail());
        assertEquals("4351037", body.getPhone());
        assertEquals("Vana-Võidu", body.getAddress().getStreet());
        assertEquals("EE", body.getAddress().getCountry());
        assertEquals("70101", body.getAddress().getPostcode());
        assertEquals("70005565", body.getBodyIds().get(0).getId());
        assertEquals(BodyIdentifier.Type.TRADE_REGISTER, body.getBodyIds().get(0).getType());
        assertEquals(BodyIdentifier.Scope.EE, body.getBodyIds().get(0).getScope());



        body = EPEParserUtils.parseBody("Kuressaare Haigla Sihtasutus, 90004059, Aia 25, Kuressaare, EE3300 Eesti (EE)"
            + " Kontaktisik: Viktor Sarapuu Tel.: 5177607 Faks: 4520005 E-post: viktor.sarapuu@saarehaigla.ee"
            + " URL: www.saarehaigla.ee Hankijaprofiili aadress: www.saarehaigla.ee");

        assertEquals("Kuressaare Haigla Sihtasutus", body.getName());
        assertEquals("Viktor Sarapuu", body.getContactName());
        assertEquals("viktor.sarapuu@saarehaigla.ee", body.getEmail());
        assertEquals("5177607", body.getPhone());
        assertEquals("Kuressaare", body.getAddress().getCity());
        assertEquals("Aia 25", body.getAddress().getStreet());
        assertEquals("EE", body.getAddress().getCountry());
        assertEquals("EE3300", body.getAddress().getPostcode());
        assertEquals("www.saarehaigla.ee", body.getAddress().getUrl());
        assertEquals("90004059", body.getBodyIds().get(0).getId());
        assertEquals(BodyIdentifier.Type.TRADE_REGISTER, body.getBodyIds().get(0).getType());
        assertEquals(BodyIdentifier.Scope.EE, body.getBodyIds().get(0).getScope());

        body = EPEParserUtils.parseBody("Kaitsevägi, 70008641, Juhkentali 58, Tallinn, 15007 Eesti (EE)"
            + " Kontaktisik: Urmas Roossaar Tel.: +372 53449137 Faks: +372 6011120 E-post: urmas.roossaar@mil.ee"
            + " URL: http://www.mil.ee");

        assertEquals("Kaitsevägi", body.getName());
        assertEquals("Urmas Roossaar", body.getContactName());
        assertEquals("urmas.roossaar@mil.ee", body.getEmail());
        assertEquals("+372 53449137", body.getPhone());
        assertEquals("Tallinn", body.getAddress().getCity());
        assertEquals("Juhkentali 58", body.getAddress().getStreet());
        assertEquals("EE", body.getAddress().getCountry());
        assertEquals("15007", body.getAddress().getPostcode());
        assertEquals("http://www.mil.ee", body.getAddress().getUrl());
        assertEquals("70008641", body.getBodyIds().get(0).getId());
        assertEquals(BodyIdentifier.Type.TRADE_REGISTER, body.getBodyIds().get(0).getType());
        assertEquals(BodyIdentifier.Scope.EE, body.getBodyIds().get(0).getScope());



        body = EPEParserUtils.parseBody("Maiken jaanisk, 47410096536 Eesti (EE)");
        assertEquals("Maiken jaanisk", body.getName());
        assertEquals("47410096536", body.getBodyIds().get(0).getId());
        assertEquals(BodyIdentifier.Type.TRADE_REGISTER, body.getBodyIds().get(0).getType());
        assertEquals(BodyIdentifier.Scope.EE, body.getBodyIds().get(0).getScope());
        assertEquals("EE", body.getAddress().getCountry());
    }

    /**
     * Tests of failed body parsing with {@link EPEParserUtils#parseBody(java.lang.String)}.
     */
    @Test(expected = UnrecoverableException.class)
    public final void parseBodyFailedTest() {
        EPEParserUtils.parseBody("");
        EPEParserUtils.parseBody(null);
        EPEParserUtils.parseBody("Maiken jaanisk");
        EPEParserUtils.parseBody("Maiken jaanisk, 47410096536");
    }
}
