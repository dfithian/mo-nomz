# Mo Nomz

## Start the server

```bash
stack run mo-nomz
```

## Set up database

```bash
initdb -D tmp
pg_ctl -D tmp -l postgres.log start
psql -d postgres -c "create user postgres"
psql -d postgres -c "grant create on database postgres to postgres"
psql -d postgres -U postgres -f sql/init.sql
```

## Seed data

```bash
curl \
  -H 'Content-Type: application/json' \
  http://localhost:8080/api/v1/recipe/import/json \
  -d '{
    "name": "Chicken Parmesan",
    "ingredients": [{
      "name": "chicken",
      "quantity": 2,
      "unit": "pounds"
    }, {
      "name": "marinara sauce",
      "quantity": 16,
      "unit": "oz"
    }, {
      "name": "mozzarella",
      "quantity": 1,
      "unit": "ball"
    }]
  }'

curl \
  -H 'Content-Type: application/json' \
  http://localhost:8080/api/v1/recipe/import/json \
  -d '{
    "name": "Eggplant Parmesan",
    "ingredients": [{
      "name": "eggplant",
      "quantity": 2,
      "unit": "large"
    }, {
      "name": "marinara sauce",
      "quantity": 16,
      "unit": "oz"
    }, {
      "name": "mozzarella",
      "quantity": 1,
      "unit": "ball"
    }]
  }'

curl \
  -H 'Content-Type: application/json' \
  http://localhost:8080/api/v1/recipe/import/link \
  -d '{
    "link": "https://www.allrecipes.com/recipe/26317/chicken-pot-pie-ix/"
  }'
```
