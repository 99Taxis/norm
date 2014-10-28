# --- !Ups

CREATE TABLE Product (
    id          bigserial primary key,
    name        varchar(25),
    description varchar(100),
    price       decimal,
    taxRange    integer,
    inStock     boolean
);

# --- !Downs

DROP TABLE Product;