# --- !Ups

CREATE TABLE Product (
    id          bigserial primary key,
    name        varchar(25),
    description varchar(100),
    price       decimal,
    taxRange    integer,
    inStock     boolean,
    createdAt   TIMESTAMP DEFAULT now(),
    updatedAt   TIMESTAMP DEFAULT now()
);

# --- !Downs

DROP TABLE Product;