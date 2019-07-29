from Crypto.Cipher import AES
import hashlib

password = b'qwerty12345'
key = hashlib.sha256(password).digest()
key = b'0123456789abcdef'

IV = 16 * '\x00'
mode = AES.MODE_CBC
encryptor = AES.new(key, mode, IV=IV)

text = b'j' * 32 + b'i' * 64
ciphertext = encryptor.encrypt(text)

decryptor = AES.new(key, mode, IV=IV)
plain = decryptor.decrypt(ciphertext)
print(plain)
