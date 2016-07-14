package sshd

import (
    "os"
    "io/ioutil"
    "crypto/rsa"
    "crypto/rand"
    "crypto/x509"
    "encoding/pem"
    "src/golang.org/x/crypto/ssh"
)

func MakeSSHKeyPair(keyPath string) error {
    privateKey, err := rsa.GenerateKey(rand.Reader, 1024)
    if err != nil {
        return err
    }

    privateKeyFile, err := os.OpenFile(keyPath, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0600)
    if err != nil {
        return err
    }
    defer privateKeyFile.Close()

    // generate and write private key as PEM
    privateKeyPEM := &pem.Block{
        Type: "RSA PRIVATE KEY",
        Bytes: x509.MarshalPKCS1PrivateKey(privateKey),
    }
    if err := pem.Encode(privateKeyFile, privateKeyPEM); err != nil {
        return err
    }

    // generate and write public key
    pub, err := ssh.NewPublicKey(&privateKey.PublicKey)
    if err != nil {
        return err
    }
    return ioutil.WriteFile(keyPath+".pub", ssh.MarshalAuthorizedKey(pub), 0644)
}
