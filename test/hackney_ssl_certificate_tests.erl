%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
-module(hackney_ssl_certificate_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").

%% public_key_info/1 tests
public_key_info_test_() ->
    {setup,
     fun() ->
         %% Generate a self-signed certificate for testing
         %% First, generate an RSA key pair
         {ok, PrivKey} = generate_rsa_key(),
         %% Create a self-signed certificate
         Cert = create_test_certificate(PrivKey),
         {PrivKey, Cert}
     end,
     fun(_) -> ok end,
     fun({_PrivKey, Cert}) ->
         [
            {"extracts SubjectPublicKeyInfo from certificate",
             fun() ->
                 PKI = hackney_ssl_certificate:public_key_info(Cert),
                 ?assertMatch(#'OTPSubjectPublicKeyInfo'{}, PKI)
             end},
            {"extracted PKI contains algorithm info",
             fun() ->
                 PKI = hackney_ssl_certificate:public_key_info(Cert),
                 #'OTPSubjectPublicKeyInfo'{algorithm = Algo} = PKI,
                 ?assertMatch(#'PublicKeyAlgorithm'{}, Algo)
             end},
            {"extracted PKI contains public key",
             fun() ->
                 PKI = hackney_ssl_certificate:public_key_info(Cert),
                 #'OTPSubjectPublicKeyInfo'{subjectPublicKey = PubKey} = PKI,
                 %% RSA public key should be a record
                 ?assertMatch(#'RSAPublicKey'{}, PubKey)
             end}
         ]
     end}.

%% Helper functions to generate test certificates

generate_rsa_key() ->
    %% Generate a 2048-bit RSA key for testing
    Key = public_key:generate_key({rsa, 2048, 65537}),
    {ok, Key}.

create_test_certificate(#'RSAPrivateKey'{modulus = N, publicExponent = E}) ->
    %% Create subject/issuer name
    Subject = {rdnSequence, [[#'AttributeTypeAndValue'{
        type = ?'id-at-commonName',
        value = {utf8String, <<"Test Certificate">>}
    }]]},

    %% Create validity period
    Now = calendar:universal_time(),
    NotBefore = format_time(Now),
    NotAfter = format_time(add_years(Now, 1)),
    Validity = #'Validity'{notBefore = NotBefore, notAfter = NotAfter},

    %% Create public key info
    PubKeyInfo = #'OTPSubjectPublicKeyInfo'{
        algorithm = #'PublicKeyAlgorithm'{
            algorithm = ?'rsaEncryption',
            parameters = 'NULL'
        },
        subjectPublicKey = #'RSAPublicKey'{modulus = N, publicExponent = E}
    },

    %% Create TBS certificate
    TBSCert = #'OTPTBSCertificate'{
        version = v3,
        serialNumber = rand:uniform(1000000),
        signature = #'SignatureAlgorithm'{
            algorithm = ?'sha256WithRSAEncryption',
            parameters = 'NULL'
        },
        issuer = Subject,
        validity = Validity,
        subject = Subject,
        subjectPublicKeyInfo = PubKeyInfo,
        extensions = []
    },

    %% Create the OTPCertificate record (we don't need a real signature for unit tests)
    #'OTPCertificate'{
        tbsCertificate = TBSCert,
        signatureAlgorithm = #'SignatureAlgorithm'{
            algorithm = ?'sha256WithRSAEncryption',
            parameters = 'NULL'
        },
        signature = <<0:256>>  %% Dummy signature for testing
    }.

format_time({{Y, M, D}, {H, Mi, S}}) ->
    {utcTime, lists:flatten(io_lib:format("~2..0w~2..0w~2..0w~2..0w~2..0w~2..0wZ",
        [Y rem 100, M, D, H, Mi, S]))}.

add_years({{Y, M, D}, Time}, Years) ->
    {{Y + Years, M, D}, Time}.
