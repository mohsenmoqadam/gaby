# 1. Upload General Objects:
### Request Pattern:
    * URL: http://IP:PORT/gaby/general/v1/upload/:ost
    * Content-Type: multipart, form-data
    * OST: Object-Storage Specific Token 
  
### Example:
```sh
curl -i -X POST -H "Content-Type: multipart/form-data" -F "image=@/tmp/general.jpg" http://IP:PORT/gaby/general/v1/upload/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ0eXBlIjoib3N0IiwidWlkIjowLCJhaWQiOjB9.2MZ7T72UyZRhozn5K6HEH7JPLmubRA_80ZNKK4-_pfk
```
### Reply Pattern:
+ OID: Object Identifier
+ If upload was successful (HTML CODE: 200): `{"verb": "done", "oid": "any-string"}`
+ If upload was unsuccessful (HTML CODE: 403): `{"verb": "error", "reason": "forbidden"}`
+ If upload was unsuccessful (HTML CODE: 500): `{"verb": "error", "reason": "internal server error"}`

# 1. Download General Objects:
### Request Pattern:
    * URL: http://IP:PORT/gaby/general/v1/download/:ost/:oid
    * OST: Object-Storage Specific Token 
    * OID: Object Identifier

### Example:
```sh
curl http://IP:PORT/gaby/general/v1/download/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ0eXBlIjoib3N0IiwidWlkIjowLCJhaWQiOjB9.2MZ7T72UyZRhozn5K6HEH7JPLmubRA_80ZNKK4-_pfk/6xznatEHwSCMqZXDAKvvxnEUGsDXfYYdHTgnhfvU
```
### ERROR Pattern:
+ OID: Object Identifier
+ If download was unsuccessful (HTML CODE: 404): `{"verb": "error", "reason": "not found"}`
+ If download was unsuccessful (HTML CODE: 403): `{"verb": "error", "reason": "forbidden"}`
+ If download was unsuccessful (HTML CODE: 500): `{"verb": "error", "reason": "internal server error"}`

# 2. Upload Avatar Image:
### Request Pattern:
    * URL: http://IP:PORT/gaby/avatar/v1/upload/:ost
    * Content-Type: multipart, form-data
    * OST: Object-Storage Specific Token 
  
### Example:
```sh
curl -i -X POST -H "Content-Type: multipart/form-data" -F "image=@/tmp/general.jpg" http://IP:PORT/gaby/avatar/v1/upload/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ0eXBlIjoib3N0IiwidWlkIjowLCJhaWQiOjB9.2MZ7T72UyZRhozn5K6HEH7JPLmubRA_80ZNKK4-_pfk
```
### Reply Pattern:
+ OID: Object Identifier
+ If upload was successful (HTML CODE: 200): `{"verb": "done", "oid": "any-string"}`
+ If upload was unsuccessful (HTML CODE: 403): `{"verb": "error", "reason": "forbidden"}`
+ If upload was unsuccessful (HTML CODE: 500): `{"verb": "error", "reason": "internal server error"}`

# 2. Download Avatar Image:
### Request Pattern:
    * URL: http://IP:PORT/gaby/avatar/v1/download/:ost/:uid
    * OST: Object-Storage Specific Token 
    * UID: User Identifier

### Example:
```sh
curl http://IP:PORT/gaby/avatar/v1/download/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ0eXBlIjoib3N0IiwidWlkIjowLCJhaWQiOjB9.2MZ7T72UyZRhozn5K6HEH7JPLmubRA_80ZNKK4-_pfk/123456789
```
### ERROR Pattern:
+ OID: Object Identifier
+ If download was unsuccessful (HTML CODE: 404): `{"verb": "error", "reason": "not found"}`
+ If download was unsuccessful (HTML CODE: 403): `{"verb": "error", "reason": "forbidden"}`
+ If download was unsuccessful (HTML CODE: 500): `{"verb": "error", "reason": "internal server error"}`

# 3. Upload Onboarding Image:
### Request Pattern:
    * URL: http://IP:PORT/gaby/onboarding/v1/upload/:ost
    * Content-Type: multipart, form-data
    * OST: Object-Storage Specific Token 
  
### Example:
```sh
curl -i -X POST -H "Content-Type: multipart/form-data" -F "image=@/tmp/general.jpg" http://IP:PORT/gaby/onboarding/v1/upload/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ0eXBlIjoib3N0IiwidWlkIjowLCJhaWQiOjB9.2MZ7T72UyZRhozn5K6HEH7JPLmubRA_80ZNKK4-_pfk
```
### Reply Pattern:
+ OID: Object Identifier
+ If upload was successful (HTML CODE: 200): `{"verb": "done", "oid": "any-string"}`
+ If upload was unsuccessful (HTML CODE: 403): `{"verb": "error", "reason": "forbidden"}`
+ If upload was unsuccessful (HTML CODE: 500): `{"verb": "error", "reason": "internal server error"}`

# 3. Download Onboarding Image:
### Request Pattern:
    * URL: http://IP:PORT/gaby/onboarding/v1/download/:oid
    * UID: Object Identifier

### Example:
```sh
curl http://IP:PORT/gaby/onboarding/v1/download/6xznatEHwSCMqZXDAKvvxnEUGsDXfYYdHTgnhfvU
```
### ERROR Pattern:
+ OID: Object Identifier
+ If download was unsuccessful (HTML CODE: 404): `{"verb": "error", "reason": "not found"}`
+ If download was unsuccessful (HTML CODE: 403): `{"verb": "error", "reason": "forbidden"}`
+ If download was unsuccessful (HTML CODE: 500): `{"verb": "error", "reason": "internal server error"}`
